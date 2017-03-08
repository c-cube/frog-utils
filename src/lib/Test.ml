
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

open Lwt.Infix

module E = Misc.Err
module R = Html.Record
module H = Html

module MStr = Misc.StrMap

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

let time_of_res e = e.Event.raw.Event.rtime

module Raw = struct
  type t = result MStr.t
  let empty = MStr.empty

  let add r raw =
    let pb = r.Event.problem.Problem.name in
    MStr.add pb r raw

  let merge =
    MStr.merge
      (fun _ a b -> if a=None then b else a)

  let of_list l = List.fold_left (fun acc r -> add r acc) empty l

  let to_yojson r : Yojson.Safe.json =
    let l =
      MStr.fold
        (fun _ r acc ->
           let j = result_to_yojson r in
           j :: acc)
        r []
    in
    `List l

  let of_yojson (j:Yojson.Safe.json) =
    let open E in
    let get_list_ = function
      | `List l -> return l
      | _ -> fail "expected list"
    in
    get_list_ j
    >|= List.map result_of_yojson
    >>= seq_list
    >|= of_list

  type stat = {
    unsat: int;
    sat: int;
    errors: int;
    unknown: int;
    timeout: (int [@default 0]);
    total_time: float; (* for sat+unsat *)
  } [@@deriving yojson]

  let stat_empty = {unsat=0; sat=0; errors=0; unknown=0; timeout=0; total_time=0.; }

  let add_sat_ t s = {s with sat=s.sat+1; total_time=s.total_time+. t; }
  let add_unsat_ t s = {s with unsat=s.unsat+1; total_time=s.total_time+. t; }
  let add_unknown_ s = {s with unknown=s.unknown+1}
  let add_error_ s = {s with errors=s.errors+1}
  let add_timeout_ s = {s with timeout=s.timeout+1}

  let pp_stat out s =
    fpf out
      "{@[<hv>unsat: %d,@ sat: %d,@ errors: %d,@ unknown: %d,@ \
       timeout: %d,@ total: %d,@ total_time: %.2f@]}"
      s.unsat s.sat s.errors s.unknown s.timeout
      (s.unsat + s.sat + s.errors + s.unknown + s.timeout)
      s.total_time

  let stat r =
    (* stats *)
    let stat = ref stat_empty in
    let add_res time res =
      stat := (match res with
          | Res.Unsat -> add_unsat_ time | Res.Sat -> add_sat_ time
          | Res.Unknown -> add_unknown_ | Res.Error -> add_error_
          | Res.Timeout -> add_timeout_
        ) !stat
    in
    MStr.iter (fun _ r -> add_res (time_of_res r) (Event.analyze_p r)) r;
    !stat

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
end

module Analyze = struct
  [@@@warning "-39"]
  type t = {
    raw: Raw.t;
    stat: Raw.stat;
    improved: result list;
    ok: result list;
    disappoint: result list;
    bad: result list;
  } [@@deriving yojson]
  [@@@warning "+39"]

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
    let stat = Raw.stat raw in
    improved, ok, bad, disappoint, stat

  let make raw =
    let improved, ok, bad, disappoint, stat = analyse_ raw in
    { raw; stat; improved; ok; disappoint; bad; }

  let of_yojson j = match Raw.of_yojson j with
    | Result.Ok x -> Result.Ok (make x)
    | Result.Error s -> Result.Error s

  let to_yojson t = Raw.to_yojson t.raw

  let of_list l =
    let raw = Raw.of_list l in
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

  let pp_list_ p = Misc.Fmt.pp_list ~start:"" ~stop:"" p

  let pp_raw_res_ out r =
    fpf out "@[<h>problem %s (expected: %a, result: %a)@]"
      r.Event.problem.Problem.name
      Res.print r.Event.problem.Problem.expected
      Res.print (Event.analyze_p r)

  let print out { raw; stat; improved; ok; disappoint; bad } =
    let pp_l out = fpf out "[@[<hv>%a@]]" (pp_list_ pp_raw_res_) in
    fpf out
      "@[<hv2>results: {@,stat:%a,@ %-15s: %a,@ %-15s: %a,@ %-15s: %a,@ %-15s: %a@]@,}"
      Raw.pp_stat stat
      "ok" pp_l ok
      "improved" pp_l improved
      "disappoint" pp_l disappoint
      "bad" pp_l bad

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
    else Raw.to_html_raw_tbl uri_of_problem uri_of_raw_res l

  let to_html uri_of_problem uri_of_raw_res t =
    let lst_raw_res ?cls l =
      if l=[] then H.pcdata "ø"
      else Raw.to_html_raw_tbl uri_of_problem uri_of_raw_res l
    in
    R.start
    |> R.add "summary" (to_html_summary t)
    |> R.add "improved" (lst_raw_res t.improved)
    |> R.add "ok" (lst_raw_res t.ok)
    |> R.add "disappoint" (lst_raw_res t.disappoint)
    |> R.add "bad" (lst_raw_res t.bad)
    |> R.add "stats" (Raw.to_html_stats t.stat)
    |> R.add "raw" (to_html_raw uri_of_problem uri_of_raw_res t.raw)
    |> R.close
end

module Config = struct
  [@@@warning "-39"]
  type expect =
    | Auto
    | Res of Res.t
    | Program of Prover.t
  [@@deriving yojson]

  type problem_set = {
    directory : string;
    pattern : string;
    expect : expect;
  } [@@deriving yojson]

  type t = {
    j: int; (* number of concurrent processes *)
    timeout: int; (* timeout for each problem *)
    memory: int;
    problems : problem_set list [@default []];
    provers: Prover.t list;
  } [@@deriving yojson]
  [@@@warning "+39"]

  let make ?(j=1) ?(timeout=5) ?(memory=1000) ?(dirs=[]) ~provers () =
    { j; timeout; memory; provers; problems=dirs; }

  let update ?j ?timeout ?memory c =
    let j = CCOpt.get_or ~default:c.j j in
    let timeout = CCOpt.get_or ~default:c.timeout timeout in
    let memory = CCOpt.get_or ~default:c.memory memory in
    { c with j; timeout; memory; }

  let to_html_expect uri_of_prover = function
    | Auto -> H.pcdata "auto"
    | Res r -> Res.to_html r
    | Program p ->
      H.a
        ~a:[H.a_href (uri_of_prover p |> Uri.to_string)]
        [H.div [Prover.to_html_name p]]

  let to_html_pb_dir uri_of_prover d : html =
    R.start
    |> R.add_string "directory" d.directory
    |> R.add_string ~raw:true "pattern" d.pattern
    |> R.add "expect" (to_html_expect uri_of_prover d.expect)
    |> R.close

  let to_record_pb_dirs uri_of_prover l : R.t =
    let aux acc = function
      | [] -> acc
      | d :: r ->
        R.add "problem set" (to_html_pb_dir uri_of_prover d) acc
    in
    aux R.start l

  let to_html uri_of_prover c : html =
    R.start
    |> R.add_int "j" c.j
    |> R.add_int "timeout" c.timeout
    |> R.add_int "memory" c.memory
    |> R.add_record (to_record_pb_dirs uri_of_prover c.problems)
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
    same: (Problem.t * Res.t * float * float) list; (* same result *)
  }

  (* TODO: use outer_join? to also find the disappeared/appeared *)
  let compare (a: Raw.t) b : t =
    let open Event in
    let module M = OLinq.AdaptMap(MStr) in
    let a = M.of_map a |> OLinq.map snd in
    let b = M.of_map b |> OLinq.map snd in
    let j =
      OLinq.join ~eq:Problem.same_name
        (fun r -> r.problem) (fun r -> r.problem) a b
        ~merge:(fun pb r1 r2 ->
            assert (r1.problem.Problem.name = r2.problem.Problem.name);
            Some (pb, analyze_p r1, analyze_p r2, time_of_res r1, time_of_res r2))
      |> OLinq.group_by (fun (_,res1,res2,_,_) -> Res.compare res1 res2)
      |> OLinq.run_list
    in
    let tup3 = List.map (fun (a,b,c,_,_) -> a,b,c) in
    let improved = assoc_or [] `RightBetter j |> tup3 in
    let regressed = assoc_or [] `LeftBetter j |> tup3 in
    let mismatch = assoc_or [] `Mismatch j |> tup3 in
    let same = assoc_or [] `Same j |> List.rev_map (fun (pb,r,_,t1,t2) -> pb,r,t1,t2) in
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
  let pp_pb_same out (pb,res,t1,t2) =
    fpf out "@[<h>%s: %a (%.2f vs %.2f)@]" pb.Problem.name Res.print res t1 t2
  let pp_pb_res2 ?(color=`Normal) ~bold out (pb,res1,res2) =
    let module F = Misc.Fmt in
    fpf out "@[<h>%s: %a@]" pb.Problem.name
      ((if bold then F.in_bold_color color else F.in_color color)
         (fun out () -> fpf out "%a -> %a" Res.print res1 Res.print res2))
      ()

  let print out (t:t) =
    let module F = Misc.Fmt in
    fpf out "@[<v2>comparison: {@,appeared: %a,@ disappeared: %a,@ same: %a \
             ,@ mismatch: %a,@ improved: %a,@ regressed: %a@]@,}"
      (pp_hvlist_ pp_pb_res) t.appeared
      (pp_hvlist_ pp_pb_res) t.disappeared
      (pp_hvlist_ pp_pb_same) t.same
      (pp_hvlist_ (pp_pb_res2 ~bold:true ~color:`Red)) t.mismatch
      (pp_hvlist_ (pp_pb_res2 ~bold:false ~color:`Green)) t.improved
      (pp_hvlist_ (pp_pb_res2 ~bold:true ~color:`Yellow)) t.regressed

  let print_short out (t:t) =
    let module F = Misc.Fmt in
    fpf out "(@[<v2>comparison@ appeared: %d,@ disappeared: %d@ same: %d \
             @ mismatch: %a@ improved: %a@ regressed: %a@])"
      (List.length t.appeared)
      (List.length t.disappeared)
      (List.length t.same)
      (pp_hvlist_ (pp_pb_res2 ~bold:true ~color:`Normal)) t.mismatch
      (pp_hvlist_ (pp_pb_res2 ~bold:false ~color:`Green)) t.improved
      (pp_hvlist_ (pp_pb_res2 ~bold:true ~color:`Yellow)) t.regressed

  let to_html _ _ = assert false (* TODO! *)
end

type top_result = {
  uuid: Uuidm.t lazy_t; (* unique ID *)
  timestamp: float; (* timestamp *)
  events: Event.t list;
  raw: Raw.t Prover.Map_name.t lazy_t;
  analyze: Analyze.t Prover.Map_name.t lazy_t;
}

module Top_result = struct
  type t = top_result

  let snapshot ?meta t =
    Event.Snapshot.make ?meta ~uuid:(Lazy.force t.uuid) t.events

  let same_uuid (a:t)(b:t): bool =
    Uuidm.equal (Lazy.force a.uuid) (Lazy.force b.uuid)

  let make ?uuid ?timestamp l =
    let uuid = match uuid with
      | Some u -> Lazy.from_val u
      | None -> lazy (Uuidm.create `V4)
    and timestamp = match timestamp with
      | None -> Unix.gettimeofday()
      | Some t -> t
    in
    let raw = lazy (
      l
      |> List.fold_left
        (fun map e -> match e with
           | Event.Prover_run r ->
             let p = r.Event.program in
             let raw =
               try Prover.Map_name.find p map with Not_found -> Raw.empty
             in
             let analyze_raw = Raw.add r raw in
             Prover.Map_name.add p analyze_raw map
           | Event.Checker_run _ -> map)
        Prover.Map_name.empty
    ) in
    let analyze = lazy (
      Prover.Map_name.map Analyze.make (Lazy.force raw)
    ) in
    { uuid; timestamp; events=l; raw; analyze; }

  let of_snapshot s =
    make ~uuid:s.Event.uuid ~timestamp:s.Event.timestamp s.Event.events

  let merge a b = make (List.rev_append a.events b.events)

  let merge_l l =
    let events = List.map (fun t->t.events) l |> List.flatten in
    make events

  let to_file ~file r =
    let snapshot = Event.Snapshot.make ~uuid:(Lazy.force r.uuid) r.events in
    Event.Snapshot.to_file ~file snapshot

  let of_file ~file =
    let open Misc.LwtErr in
    Event.Snapshot.of_file ~file >|= of_snapshot

  let pp_header out t =
    Format.fprintf out "(@[(uuid %s)@ (date %a)@])"
      (Uuidm.to_string (Lazy.force t.uuid))
      ISO8601.Permissive.pp_datetime t.timestamp

  let pp out (r:t) =
    let pp_tup out (p,res) =
      Format.fprintf out "@[<2>%a:@ @[%a@]@]"
        Prover.pp_name p Analyze.print res
    in
    let {analyze=lazy a; uuid=lazy u; _} = r in
    Format.fprintf out "(@[<2>%a@ @[<v>%a@]@])"
      pp_header r (Misc.Fmt.pp_list pp_tup) (Prover.Map_name.to_list a)

  type comparison_result = {
    both: ResultsComparison.t Prover.Map_name.t;
    left: Analyze.t Prover.Map_name.t;
    right: Analyze.t Prover.Map_name.t;
  }

  let compare (a:t) (b:t): comparison_result =
    let {analyze=lazy a; _} = a in
    let {analyze=lazy b; _} = b in
    let both, left =
      Prover.Map_name.fold
        (fun p r_left (both,left) ->
           try
             (* find same (problem,dir) in [b], and compare *)
             let r_right = Prover.Map_name.find p b in
             let cmp =
               ResultsComparison.compare r_left.Analyze.raw r_right.Analyze.raw
             in
             (p, cmp) :: both, left
           with Not_found ->
             both, (p,r_left)::left)
        a ([],[])
    in
    let right =
      Prover.Map_name.filter
        (fun p _ -> not (Prover.Map_name.mem p a))
        b
    in
    let both = Prover.Map_name.of_list both in
    let left = Prover.Map_name.of_list left in
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
      (Misc.Fmt.pp_list pp_tup) (Prover.Map_name.to_list r.both)
      (Misc.Fmt.pp_list (pp_one "left")) (Prover.Map_name.to_list r.left)
      (Misc.Fmt.pp_list (pp_one "right")) (Prover.Map_name.to_list r.right)

  module StrSet = Misc.StrSet

  type table_row = {
    tr_problem: string;
    tr_res: (string * Res.t * float) list; (* prover, result, time *)
  }

  type table = {
    t_meta: string;
    t_rows: table_row list;
    t_provers: string list;
  }

  let to_table (t:t): table =
    let lazy map = t.analyze in
    let find_cell (t:t) p pb : result option =
      try Some (MStr.find pb (Prover.Map_name.find p map).Analyze.raw)
      with Not_found -> None
    in
    let line0 =
      Printf.sprintf "snapshot %s at %s"
        (Uuidm.to_string (Lazy.force t.uuid))
        (ISO8601.Permissive.string_of_datetime t.timestamp)
    in
    let provers = Prover.Map_name.to_list map |> List.map fst in
    let all_problems : StrSet.t =
      Prover.Map_name.fold
        (fun _ analyze acc ->
           MStr.fold (fun pb _ acc -> StrSet.add pb acc) analyze.Analyze.raw acc)
        map
        StrSet.empty
    in
    let t_rows =
      StrSet.fold
        (fun file acc ->
           let tr_res =
             List.map
               (fun prover -> match find_cell t prover file with
                  | None ->
                    Prover.name prover, Res.Unknown, 0.
                  | Some res ->
                    let time = time_of_res res in
                    let res = Event.analyze_p res in
                    Prover.name prover, res, time)
               provers
           in
           {tr_problem=file; tr_res} :: acc)
        all_problems []
    in
    {t_meta=line0; t_provers=List.map Prover.name provers; t_rows}

  let table_to_csv (t:table): Csv.t =
    let time_to_csv (r:Res.t) f = match r with
      | Res.Timeout | Res.Error | Res.Unknown -> "-"
      | Res.Sat | Res.Unsat -> Printf.sprintf "%.2f" f
    and res_to_csv (r:Res.t) = match r with
      | Res.Error -> "error"
      | Res.Timeout -> "timeout"
      | Res.Unknown -> "unknown"
      | Res.Sat -> "sat"
      | Res.Unsat -> "unsat"
    in
    let line0 = [t.t_meta] in
    let header_line = "problem" :: t.t_provers @ t.t_provers in
    let lines =
      List.map
        (fun r ->
           r.tr_problem
           :: List.map (fun (_,res,_) ->  res_to_csv res) r.tr_res
           @ List.map (fun (_,res,t) -> time_to_csv res t) r.tr_res)
        t.t_rows
    in
    line0 :: header_line :: lines

  let to_csv t : Csv.t =
    table_to_csv (to_table t)

  let to_csv_chan oc t =
    let chan = Csv.to_channel oc in
    Csv.output_all chan (to_csv t)

  let to_csv_file file t =
    let oc = open_out file in
    to_csv_chan oc t;
    close_out oc

  let to_csv_string t =
    let buf = Buffer.create 256 in
    let ch = Csv.to_buffer buf in
    Csv.output_all ch (to_csv t);
    Buffer.contents buf
end

(** {2 Benchmark, within one Top Result} *)
module Bench = struct
  type per_prover = {
    stat: Raw.stat;
    sat: (string * float) list;
    unsat: (string * float) list;
  }

  type t = {
    from: top_result;
    per_prover: per_prover Prover.Map_name.t;
  }

  let make (r:top_result): t =
    let per_prover =
      Prover.Map_name.map
        (fun raw ->
           let stat = Raw.stat raw in
           let sat =
             MStr.fold
               (fun file res acc -> match Event.analyze_p res with
                  | Res.Sat -> (file, time_of_res res) :: acc
                  | _ -> acc)
               raw []
           and unsat =
             MStr.fold
               (fun file res acc -> match Event.analyze_p res with
                  | Res.Unsat -> (file, time_of_res res) :: acc
                  | _ -> acc)
               raw []
           in
           {stat; sat; unsat})
        (Lazy.force r.raw)
    in
    {from=r; per_prover}

  let pp out (r:t): unit =
    let pp_stat out (p,per_prover) =
      Format.fprintf out "@[<h2>%a:@ %a@]"
        Prover.pp_name p Raw.pp_stat per_prover.stat
    and pp_full out (_p,_res) =
      () (* TODO *)
    in
    let l = Prover.Map_name.to_list r.per_prover in
    Format.fprintf out "(@[<v2>%s@ @[%a@]@ @[<v>%a@]@])"
      (Uuidm.to_string (Lazy.force r.from.uuid))
      (Misc.Fmt.pp_list ~sep:"" pp_stat) l
      (Misc.Fmt.pp_list ~sep:"" pp_full) l
end

(** {2 Compare a {!Top_result.t} with others} *)
module Summary = struct
  type individual_diff = {
    wrt: Top_result.t;
    raw_comparison: ResultsComparison.t Prover.Map_name.t; (* not empty *)
  }

  type regression_by_prover = {
    reg_prover: Prover.t;
    reg_res: (Problem.t * Res.t * Res.t) list;
  }

  (* a summary of regression *)
  type regression = {
    reg_wrt: Top_result.t;
    reg_by_prover: regression_by_prover list; (* not empty *)
  }

  type t = {
    main: Top_result.t;
    others: individual_diff list;
    regressions: regression list;
  }

  let compare_to_ (a:top_result)(other:top_result): individual_diff option * regression option =
    if Top_result.same_uuid a other then None, None
    else (
      let r = Top_result.compare other a in
      if Prover.Map_name.is_empty r.Top_result.both
      then None, None
      else (
        let o = { wrt=other; raw_comparison=r.Top_result.both; } in
        let regs_by_prover =
          Prover.Map_name.fold
            (fun prover comp acc ->
               let res = comp.ResultsComparison.regressed in
               if res=[] then acc
               else
                 let reg = {reg_prover=prover; reg_res=res} in
                 reg :: acc)
            r.Top_result.both []
        in
        let regs =
          if regs_by_prover = [] then None
          else Some {reg_wrt=other; reg_by_prover=regs_by_prover}
        in
        Some o, regs
      )
    )

  let make res l =
    let others, regressions =
      List.fold_left
        (fun (others,regressions) wrt ->
           let o, r = compare_to_ res wrt in
           let others = CCList.cons_maybe o others in
           let regressions = CCList.cons_maybe r regressions in
           others, regressions)
        ([], []) l
    in
    { main=res; others=List.rev others; regressions=List.rev regressions; }

  let pp_list p = Misc.Fmt.pp_list p

  let pp_diff out (i:individual_diff) =
    let pp_tup out (p,cmp) =
      Format.fprintf out "@[<2>%s:@ @[%a@]@]"
        (Prover.name p) ResultsComparison.print_short cmp
    in
    Format.fprintf out "(@[<2>compare_to %a@ (@[<v>%a@])@])"
      Top_result.pp_header i.wrt
      (pp_list pp_tup) (Prover.Map_name.to_list i.raw_comparison)

  let pp_reg_by_prover out (r:regression_by_prover) =
    Format.fprintf out "(@[<hv2>prover %a@ (@[<v>%a@])@])"
      Prover.pp_name r.reg_prover
      (pp_list (ResultsComparison.pp_pb_res2 ~bold:true ~color:`Yellow)) r.reg_res

  let pp_reg out (r:regression) =
    Format.fprintf out "(@[<hv2>%a@ (@[<v>%a@])@])"
      Top_result.pp_header r.reg_wrt (pp_list pp_reg_by_prover) r.reg_by_prover

  let print out (t:t) =
    Format.fprintf out "(@[summary %a@ (@[<hv>%a@])@ (@[<hv2>regressions@ %a@])@])"
      Top_result.pp_header t.main
      (pp_list pp_diff) t.others
      (pp_list pp_reg) t.regressions
end
