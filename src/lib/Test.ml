
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

open Lwt.Infix

module E = Misc.Err
module R = Html.Record
module H = Html

module Res = Res
module Problem = Problem
module ProblemSet = ProblemSet

module MStr = Map.Make(String)

type result = Event.prover Event.result
  [@@deriving yojson]

type 'a or_error = 'a E.t
type 'a printer = Format.formatter -> 'a -> unit
type html = Html.t
type uri = Uri.t

let fpf = Format.fprintf
let spf = Format.asprintf

let assoc_or def x l =
  try List.assoc x l
  with Not_found -> def

module Analyze = struct
  type raw = result MStr.t

  let empty_raw = MStr.empty

  let add_raw r raw =
    let pb = r.Event.problem.Problem.name in
    MStr.add pb r raw

  let raw_of_list l = List.fold_left (fun acc r -> add_raw r acc) empty_raw l

  let raw_to_yojson r : Yojson.Safe.json =
    let l =
      MStr.fold
        (fun _ r acc ->
           let j = result_to_yojson r in
           j :: acc)
        r []
    in
    `List l

  let raw_of_yojson (j:Yojson.Safe.json) =
    let open E in
    let get_list_ = function
      | `List l -> return l
      | _ -> fail "expected list"
    in
    get_list_ j
    >|= List.map result_of_yojson
    >>= seq_list
    >|= raw_of_list

  type stat = {
    unsat: int;
    sat: int;
    errors: int;
    unknown: int;
    timeout: (int [@default 0]);
  } [@@deriving yojson]

  let stat_empty = {unsat=0; sat=0; errors=0; unknown=0; timeout=0; }

  let add_sat_ s = {s with sat=s.sat+1}
  let add_unsat_ s = {s with unsat=s.unsat+1}
  let add_unknown_ s = {s with unknown=s.unknown+1}
  let add_error_ s = {s with errors=s.errors+1}
  let add_timeout_ s = {s with timeout=s.timeout+1}

  let pp_stat out s =
    fpf out
      "{@[<hv>unsat: %d,@ sat: %d,@ errors: %d,@ unknown: %d,@ \
       timeout: %d,@ total: %d@]}"
      s.unsat s.sat s.errors s.unknown s.timeout
      (s.unsat + s.sat + s.errors + s.unknown + s.timeout)

  type t = {
    raw: raw;
    stat: stat;
    improved: result list;
    ok: result list;
    disappoint: result list;
    bad: result list;
  } [@@deriving yojson]

  let merge_raw =
    MStr.merge
      (fun _ a b -> if a=None then b else a)

  let analyse_ raw =
    let module M = OLinq.AdaptMap(MStr) in
    let l =
      M.of_map raw
      |> OLinq.map snd
      |> OLinq.group_by
        (fun r -> Problem.compare_res r.Event.problem
            (Event.analyze_p r))
      |> OLinq.run_list ?limit:None
    in
    let improved = assoc_or [] `Improvement l in
    let ok = assoc_or [] `Same l in
    let bad = assoc_or [] `Mismatch l in
    let disappoint = assoc_or [] `Disappoint l in
    (* stats *)
    let stat = ref stat_empty in
    let add_res res =
      stat := (match res with
          | Res.Unsat -> add_unsat_ | Res.Sat -> add_sat_
          | Res.Unknown -> add_unknown_ | Res.Error -> add_error_
          | Res.Timeout -> add_timeout_
        ) !stat
    in
    MStr.iter (fun _ r -> add_res (Event.analyze_p r)) raw;
    improved, ok, bad, disappoint, !stat

  let add_raw r raw =
    MStr.add r.Event.problem.Problem.name r raw

  let make raw =
    let improved, ok, bad, disappoint, stat = analyse_ raw in
    { raw; stat; improved; ok; disappoint; bad; }

  let of_yojson j = match raw_of_yojson j with
    | Result.Ok x -> Result.Ok (make x)
    | Result.Error s -> Result.Error s

  let to_yojson t = raw_to_yojson t.raw

  let of_list l =
    let raw = raw_of_list l in
    make raw

  let of_file ~file =
    try
      let json = Yojson.Safe.from_file file in
      of_yojson json
    with e -> E.fail (Printexc.to_string e)

  let to_file ~file t = Yojson.Safe.to_file file (to_yojson t)

  (* build statistics and list of mismatch from raw results *)

  let is_ok r = r.bad = []
  let num_failed r = List.length r.bad

  let pp_list_ p = Misc.Fmt.pp_list p

  let pp_raw_res_ out r =
    fpf out "@[<h>problem %s (expected: %a, result: %a)@]"
      r.Event.problem.Problem.name
      Res.print r.Event.problem.Problem.expected
      Res.print (Event.analyze_p r)

  let print out r =
    let pp_l out = fpf out "[@[<hv>%a@]]" (pp_list_ pp_raw_res_) in
    fpf out
      "@[<hv2>results: {@,stat:%a,@ %-15s: %a,@ %-15s: %a,@ %-15s: %a@]@,}"
      pp_stat r.stat
      "ok" pp_l r.ok
      "disappoint" pp_l r.disappoint
      "bad" pp_l r.bad

  let to_html_stats s =
    R.start
    |> R.add_int "unsat" s.unsat
    |> R.add_int "sat" s.sat
    |> R.add_int "errors" s.errors
    |> R.add_int "unknown" s.unknown
    |> R.close

  let to_html_raw_result_l uri_of_problem uri_of_raw_res r =
    [ H.div [H.a
        ~a:[H.a_href (uri_of_problem r.Event.problem)]
        [H.div [Problem.to_html_name r.Event.problem]]]
    ; H.div [H.a ~a:[H.a_href (uri_of_raw_res r)]
               [H.div[Res.to_html @@ Event.analyze_p r]]]
    ]

  let to_html_raw_tbl uri_of_problem uri_of_raw_res l =
    H.table
      (H.tr [H.th [H.pcdata "problem"]; H.th [H.pcdata "result"]]
       ::
         (List.rev_map
            (fun r ->
               H.tr (List.map (fun d->H.td [d])
                   (to_html_raw_result_l uri_of_problem uri_of_raw_res r)))
            l))

  let to_html_summary t =
    H.table
      (List.map
         (fun (s, i) ->
            H.tr [H.td [H.pcdata s]; H.td [H.pcdata (string_of_int i)]])
         [
           "ok", (List.length t.ok);
           "improved", (List.length t.improved);
           "disappoint", (List.length t.disappoint);
           "bad", (List.length t.bad);
           "total", (MStr.cardinal t.raw);
         ])

  let to_html_raw uri_of_problem uri_of_raw_res r =
    let l = MStr.fold (fun _ r acc -> r::acc) r [] in
    if l = [] then H.pcdata "ø"
    else to_html_raw_tbl uri_of_problem uri_of_raw_res l

  let to_html uri_of_problem uri_of_raw_res t =
    let lst_raw_res ?cls l =
      if l=[] then H.pcdata "ø"
      else to_html_raw_tbl uri_of_problem uri_of_raw_res l
    in
    R.start
    |> R.add "summary" (to_html_summary t)
    |> R.add "improved" (lst_raw_res t.improved)
    |> R.add "ok" (lst_raw_res t.ok)
    |> R.add "disappoint" (lst_raw_res t.disappoint)
    |> R.add "bad" (lst_raw_res t.bad)
    |> R.add "stats" (to_html_stats t.stat)
    |> R.add "raw" (to_html_raw uri_of_problem uri_of_raw_res t.raw)
    |> R.close
end

module Config = struct
  [@@@warning "-39"]
  type t = {
    j: int; (* number of concurrent processes *)
    timeout: int; (* timeout for each problem *)
    memory: int;
    default_dirs : (string list [@default []]);
    default_expect: (Res.t option [@default None]); (* default status for problems *)
    problem_pat: string; (* regex for problems *)
    provers: Prover.t list;
  } [@@deriving yojson]
  [@@@warning "+39"]

  let make ?(j=1) ?(timeout=5) ?(memory=1000) ?(dir=[]) ?default_expect ~pat ~provers () =
    { j; timeout; memory; provers; default_dirs=dir; default_expect; problem_pat=pat; }

  let update ?j ?timeout ?memory c =
    let module O = Misc.Opt in
    let j = O.get c.j j in
    let timeout = O.get c.timeout timeout in
    let memory = O.get c.memory memory in
    { c with j; timeout; memory; }

  let of_file file =
    let module C = Config in
    Lwt_log.ign_debug_f "parse config file `%s`..." file;
    try
      let main = C.parse_files [file] C.empty in
      let c = C.get_table main "test" in
      let j = C.get_int ~default:1 c "parallelism" in
      let timeout = C.get_int ~default:5 c "timeout" in
      let memory = C.get_int ~default:1000 c "memory" in
      let default_dirs = C.get_string_list ~default:[] c "dir" in
      let default_expect =
        let s = C.get_string ~default:"" c "default_expect" in
        if s="" then None
        else (
          let r = Res.of_string s in
          Lwt_log.ign_debug_f "default_expect=%s" (Res.to_string r);
          Some r
        )
      in
      let problem_pat = C.get_string c "problems" in
      let provers = C.get_string_list c "provers" in
      let provers = List.map (Prover.build_from_config main) provers in
      E.return { j; timeout; memory; provers; default_expect;
                 default_dirs; problem_pat; }
    with
    | C.Error e ->
      E.fail e

  let to_html uri_of_prover c : html =
    R.start
    |> R.add_int "j" c.j
    |> R.add_int "timeout" c.timeout
    |> R.add_int "memory" c.memory
    |> R.add_string ~raw:true "problems pattern" c.problem_pat
    |> R.add "dir" (H.ul (List.map (fun s->H.li [H.pcdata s]) c.default_dirs))
    |> R.add "provers"
      (H.ul @@
       List.map
          (fun x ->
            H.li [H.a ~a:[H.a_href (uri_of_prover x |> Uri.to_string)]
                         [H.div [Prover.to_html_name x]]])
          c.provers)
    |> R.close
end

module ResultsComparison = struct
  type t = {
    appeared: (Problem.t * Res.t) list;  (* new problems *)
    disappeared: (Problem.t * Res.t) list; (* problems that disappeared *)
    improved: (Problem.t * Res.t * Res.t) list;
    regressed: (Problem.t * Res.t * Res.t) list;
    mismatch: (Problem.t * Res.t * Res.t) list;
    same: (Problem.t * Res.t) list; (* same result *)
  }

  (* TODO: use outer_join? to also find the disappeared/appeared *)
  let compare (a: Analyze.raw) b : t =
    let open Event in
    let module M = OLinq.AdaptMap(MStr) in
    let a = M.of_map a |> OLinq.map snd in
    let b = M.of_map b |> OLinq.map snd in
    let j =
      OLinq.join ~eq:Problem.same_name
        (fun r -> r.problem) (fun r -> r.problem) a b
        ~merge:(fun pb r1 r2 ->
            assert (r1.problem.Problem.name = r2.problem.Problem.name);
            Some (pb, analyze_p r1, analyze_p r2))
      |> OLinq.group_by (fun (_,res1,res2) -> Res.compare res1 res2)
      |> OLinq.run_list
    in
    let improved = assoc_or [] `RightBetter j in
    let regressed = assoc_or [] `LeftBetter j in
    let mismatch = assoc_or [] `Mismatch j in
    let same = assoc_or [] `Same j |> List.rev_map (fun (pb,r,_) -> pb,r) in
    let disappeared =
      OLinq.diff ~cmp:(fun r1 r2 -> Problem.compare_name r1.problem r2.problem) a b
      |> OLinq.map (fun r -> r.problem, analyze_p r)
      |> OLinq.run_list
    and appeared =
      OLinq.diff ~cmp:(fun r1 r2 -> Problem.compare_name r1.problem r2.problem) b a
      |> OLinq.map (fun r -> r.problem, analyze_p r)
      |> OLinq.run_list
    in
    { appeared; disappeared; mismatch; same; regressed; improved; }

  let fpf = Format.fprintf
  let pp_list_ p = Misc.Fmt.pp_list p
  let pp_hvlist_ p out = fpf out "[@[<hv>%a@]]" (pp_list_ p)
  let pp_pb_res out (pb,res) = fpf out "@[<h>%s: %a@]" pb.Problem.name Res.print res
  let pp_pb_res2 ~bold ~color out (pb,res1,res2) =
    let module F = Misc.Fmt in
    fpf out "@[<h>%s: %a@]" pb.Problem.name
      ((if bold then F.in_bold_color color else F.in_color color)
         (fun out () -> fpf out "%a -> %a" Res.print res1 Res.print res2))
      ()

  let print out t =
    let module F = Misc.Fmt in
    fpf out "@[<v2>comparison: {@,appeared: %a,@ disappeared: %a,@ same: %a \
             ,@ mismatch: %a,@ improved: %a,@ regressed: %a@]@,}"
      (pp_hvlist_ pp_pb_res) t.appeared
      (pp_hvlist_ pp_pb_res) t.disappeared
      (pp_hvlist_ pp_pb_res) t.same
      (pp_hvlist_ (pp_pb_res2 ~bold:true ~color:`Red)) t.mismatch (* RED *)
      (pp_hvlist_ (pp_pb_res2 ~bold:false ~color:`Green)) t.improved (* GREEN *)
      (pp_hvlist_ (pp_pb_res2 ~bold:true ~color:`Yellow)) t.regressed (* YELLOW *)

  let to_html _ _ = assert false (* TODO! *)
end

type top_result = {
  events: Event.t list;
  analyze: Analyze.t Prover.Map.t lazy_t;
}

module Top_result = struct
  type t = top_result

  let snapshot t = Event.Snapshot.make t.events

  let make l =
    let analyze = lazy (
      l
      |> List.fold_left
        (fun map e -> match e with
           | Event.Prover_run r ->
             let p = r.Event.program in
             let raw =
               try Prover.Map.find p map with Not_found -> Analyze.empty_raw
             in
             let analyze_raw = Analyze.add_raw r raw in
             Prover.Map.add p analyze_raw map
           | Event.Checker_run _ -> map)
        Prover.Map.empty
      |> Prover.Map.map Analyze.make
    ) in
    { events=l; analyze; }

  let of_snapshot s = make s.Event.events

  let merge a b = make (List.rev_append a.events b.events)

  let merge_l l =
    let events = List.map (fun t->t.events) l |> List.flatten in
    make events

  let to_file ~file r =
    let snapshot = Event.Snapshot.make r.events in
    Event.Snapshot.to_file ~file snapshot

  let of_file ~file =
    let open Misc.LwtErr in
    Event.Snapshot.of_file ~file >|= of_snapshot

  let pp out (r:t) =
    let pp_tup out (p,res) =
      Format.fprintf out "@[<2>%s:@ @[%a@]@]"
        (Prover.name p) Analyze.print res
    in
    let {analyze=lazy a; _} = r in
    Format.fprintf out "@[<v>%a@]" (Misc.Fmt.pp_list pp_tup)
      (Prover.Map.to_list a)

  type comparison_result = {
    both: ResultsComparison.t Prover.Map.t;
    left: Analyze.t Prover.Map.t;
    right: Analyze.t Prover.Map.t;
  }

  let compare (a:t) (b:t): comparison_result =
    let {analyze=lazy a; _} = a in
    let {analyze=lazy b; _} = b in
    let both, left =
      Prover.Map.fold
        (fun p r_left (both,left) ->
           try
             (* find same (problem,dir) in [b], and compare *)
             let r_right = Prover.Map.find p b in
             let cmp =
               ResultsComparison.compare r_left.Analyze.raw r_right.Analyze.raw
             in
             (p, cmp) :: both, left
           with Not_found ->
             both, (p,r_left)::left)
        a ([],[])
    in
    let right =
      Prover.Map.filter
        (fun p _ -> not (Prover.Map.mem p a))
        b
    in
    let both = Prover.Map.of_list both in
    let left = Prover.Map.of_list left in
    { both; left; right; }

  let pp_comparison out (r:comparison_result) =
    let pp_tup out (p,cmp) =
      Format.fprintf out "@[<2>%s:@ @[%a@]@]"
        (Prover.name p) ResultsComparison.print cmp
    and pp_one which out (p,res) =
      Format.fprintf out "@[<2>%s (only on %s):@ @[%a@]@]"
        (Prover.name p) which Analyze.print res
    in
    Format.fprintf out "@[<hv>%a@,%a@,%a@]@."
      (Misc.Fmt.pp_list pp_tup) (Prover.Map.to_list r.both)
      (Misc.Fmt.pp_list (pp_one "left")) (Prover.Map.to_list r.left)
      (Misc.Fmt.pp_list (pp_one "right")) (Prover.Map.to_list r.right)
end
