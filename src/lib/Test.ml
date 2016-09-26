
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

module Prover = FrogProver
module E = FrogMisc.Err
module W = FrogWeb
module R = W.Record
module H = W.Html

module Res = FrogRes
module Problem = FrogProblem
module ProblemSet = FrogProblemSet

module MStr = Map.Make(String)

type result = FrogRun.prover FrogRun.result
  [@@deriving yojson]

type 'a or_error = 'a E.t
type 'a printer = Format.formatter -> 'a -> unit
type html = FrogWeb.html
type uri = FrogWeb.uri

let fpf = Format.fprintf
let spf = Format.asprintf

let assoc_or def x l =
  try List.assoc x l
  with Not_found -> def

module Analyze = struct
  type raw = result MStr.t

  let raw_of_list l =
    List.fold_left
      (fun acc r -> MStr.add r.FrogRun.problem.Problem.name r acc)
      MStr.empty l

  let raw_to_yojson r : Yojson.Safe.json =
    let l = MStr.fold
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
      "{@[<hv>unsat: %d,@ sat: %d,@ errors: %d,@ unknown: %d,@ timeout: %d,@ total: %d@]}"
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

  let maki =
    let of_yojson x = E.to_exn (of_yojson x) in
    Maki_yojson.make "results"
      ~to_yojson
      ~of_yojson

  let analyse_ raw =
    let module M = OLinq.AdaptMap(MStr) in
    let l =
      M.of_map raw
      |> OLinq.map snd
      |> OLinq.group_by
        (fun r -> Problem.compare_res r.FrogRun.problem
            (FrogRun.analyze_p r))
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
    MStr.iter (fun _ r -> add_res (FrogRun.analyze_p r)) raw;
    improved, ok, bad, disappoint, !stat

  let add_raw raw r =
    MStr.add r.FrogRun.problem.Problem.name r raw

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

  let pp_list_ p = FrogMisc.Fmt.pp_list p

  let pp_raw_res_ out r =
    fpf out "@[<h>problem %s (expected: %a, result: %a)@]"
      r.FrogRun.problem.Problem.name
      Res.print r.FrogRun.problem.Problem.expected
      Res.print (FrogRun.analyze_p r)

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
    [ H.a
        ~href:(uri_of_problem r.FrogRun.problem)
        (Problem.to_html_name r.FrogRun.problem)
    ; H.a ~href:(uri_of_raw_res r) (Res.to_html @@ FrogRun.analyze_p r)
    ]

  let to_html_summary t =
    H.Create.table
      ~flags:[H.Create.Tags.Headings_fst_col]
      ~row:(fun (s, i) -> [H.string s; H.int i])
      [
        "ok", (List.length t.ok);
        "improved", (List.length t.improved);
        "disappoint", (List.length t.disappoint);
        "bad", (List.length t.bad);
        "total", (MStr.cardinal t.raw);
      ]

  let to_html_raw uri_of_problem uri_of_raw_res r =
    let l = MStr.fold (fun _ r acc -> `Row r::acc) r [] in
    if l = [] then H.string "ø"
    else H.Create.table ~flags:[H.Create.Tags.Headings_fst_row]
        ~row:(function
            | `Head -> [H.string "problem"; H.string "result"]
            | `Row r -> to_html_raw_result_l uri_of_problem uri_of_raw_res r)
        (`Head :: l)

  let to_html uri_of_problem uri_of_raw_res t =
    let lst_raw_res ?cls l =
      if l=[] then H.string "ø"
      else H.Create.table l ~flags:[]
          ~row:(to_html_raw_result_l uri_of_problem uri_of_raw_res)
           |> H.div ?cls
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

  let to_junit t : Junit.Testsuite.t =
    let module J = Junit in
    let l =
      MStr.fold
        (fun _ r acc ->
           let `Prover prover = r.FrogRun.program in
           let res = FrogRun.analyze_p r in
           let name =
             Printf.sprintf "prover `%s` on problem `%s`"
               prover.Prover.name
               r.FrogRun.problem.Problem.name
           and message =
             Printf.sprintf "result: `%s`, expected: `%s`"
               (FrogRes.to_string res)
               (FrogRes.to_string r.FrogRun.problem.Problem.expected)
           and classname = ""
           and typ = ""
           and time = r.FrogRun.raw.FrogRun.rtime
           in
           let case =
             match Problem.compare_res r.FrogRun.problem res with
               | `Mismatch ->
                 J.Testcase.error
                   ~typ ~classname ~time
                   ~name
                   ~message:""
                   message
               | `Disappoint
               | `Same
               | `Improvement ->
                 J.Testcase.pass
                   ~classname ~time
                   ~name
           in
           case :: acc)
        t.raw
        []
    in
    let suite =
      J.Testsuite.make
        ?package:None
        ?timestamp:None
        ?hostname:None
        ?system_out:None
        ?system_err:None
        ~name:"frogtest"
    in
    J.Testsuite.add_testcases l suite

  let junit_to_file suites file =
    let report = Junit.make suites in
    let xml_report = Junit.to_xml report in
    let oc = open_out file in
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "@[%a@]@." (Tyxml.Xml.pp ()) xml_report;
    close_out oc;
    ()
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

  let maki : t Maki.Value.ops =
    let module V = Maki.Value in
    let of_yojson x = E.to_exn (of_yojson x) in
    let json =  Maki_yojson.make ~of_yojson ~to_yojson "config_json" in
    V.map ~descr:"config"
      (fun t -> t, t.provers)
      (fun (t,_) -> t)
      (V.pair json (V.set FrogProver.maki))

  let make ?(j=1) ?(timeout=5) ?(memory=1000) ?(dir=[]) ?default_expect ~pat ~provers () =
    { j; timeout; memory; provers; default_dirs=dir; default_expect; problem_pat=pat; }

  let update ?j ?timeout ?memory c =
    let module O = FrogMisc.Opt in
    let j = O.get c.j j in
    let timeout = O.get c.timeout timeout in
    let memory = O.get c.memory memory in
    { c with j; timeout; memory; }

  let of_file file =
    let module C = FrogConfig in
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
      E.return { j; timeout; memory; provers; default_expect; default_dirs; problem_pat; }
    with
    | C.Error e ->
      E.fail e

  let to_html uri_of_prover c =
    let module H = W.Html in
    let module R = W.Record in
    R.start
    |> R.add_int "j" c.j
    |> R.add_int "timeout" c.timeout
    |> R.add_int "memory" c.memory
    |> R.add_string ~raw:true "problems pattern" c.problem_pat
    |> R.add "dir" (H.list (List.map H.string c.default_dirs))
    |> R.add "provers"
      (H.list @@ List.map (
          fun x ->
            H.p @@ (H.a ~href:(uri_of_prover x) (Prover.to_html_name x)))
          c.provers)
    |> R.close

  let add_server s c =
    let uri_of_prover = W.Server.get s Prover.k_uri in
    let handle _ =
      W.Server.return_html (to_html uri_of_prover c)
    in
    W.Server.add_route s ~descr:"configuration" "/config" handle;
    ()
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
    let open FrogRun in
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
  let pp_list_ p = FrogMisc.Fmt.pp_list p
  let pp_hvlist_ p out = fpf out "[@[<hv>%a@]]" (pp_list_ p)
  let pp_pb_res out (pb,res) = fpf out "@[<h>%s: %a@]" pb.Problem.name Res.print res
  let pp_pb_res2 ~bold ~color out (pb,res1,res2) =
    let module F = FrogMisc.Fmt in
    fpf out "@[<h>%s: %a@]" pb.Problem.name
      ((if bold then F.in_bold_color color else F.in_color color)
         (fun out () -> fpf out "%a -> %a" Res.print res1 Res.print res2))
      ()

  let print out t =
    let module F = FrogMisc.Fmt in
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

(* run one particular test *)
let run_pb_ ~config prover pb =
  Lwt_log.ign_debug_f "running %-15s/%-30s..."
    (Filename.basename prover.FrogProver.binary) pb.Problem.name;
  (* spawn process *)
  let%lwt result = FrogRun.run_prover
      ~timeout:config.Config.timeout
      ~memory:config.Config.memory
      ~prover ~pb ()
  in
  Lwt_log.ign_debug_f "output for %s/%s: `%s`, `%s`, errcode %d"
    prover.FrogProver.binary pb.Problem.name
    result.FrogRun.raw.FrogRun.stdout
    result.FrogRun.raw.FrogRun.stderr
    result.FrogRun.raw.FrogRun.errcode;
  Lwt.return (result :> FrogRun.program FrogRun.result)

let run_pb ?(caching=true) ?limit ~config prover pb =
  let module V = Maki.Value in
  Maki.call_exn
    ?limit
    ~bypass:(not caching)
    ~lifetime:(`KeepFor Maki.Time.(days 2))
    ~deps:[V.pack V.int config.Config.timeout;
           V.pack V.int config.Config.memory;
           V.pack Prover.maki prover;
           V.pack Problem.maki pb]
    ~op:FrogRun.maki_result
    ~name:"frogtest.run_pb"
    (fun () -> run_pb_ ~config prover pb)

let nop_ _ = Lwt.return_unit

let run ?(on_solve = nop_) ?(on_done = nop_)
    ?(caching=true) ?j ?timeout ?memory ?db ?server ?provers ~config set
  =
  let config = Config.update ?j ?timeout ?memory config in
  let limit = Maki.Limit.create config.Config.j in
  FrogMisc.Opt.iter server
    ~f:(fun s ->
        Prover.add_server s;
        Problem.add_server s;
        FrogRun.add_server s;
        Config.add_server s config;
        List.iter (fun x -> W.Server.get s Prover.k_add x) config.Config.provers;
      );
  let provers = match provers with
    | None -> config.Config.provers
    | Some l ->
      List.filter
        (fun p -> List.mem (Prover.name p) l)
        config.Config.provers
  in
  let%lwt res =
    Lwt_list.map_p
      (fun prover ->
         let%lwt l =
           Lwt_list.map_p (fun pb ->
             let%lwt result = run_pb ~caching ~limit ~config prover pb in
             begin match result with
               | { FrogRun.program = `Prover _; _ } as t ->
                 let%lwt () = on_solve t in
                 (* add result to db? *)
                 FrogMisc.Opt.iter db
                   ~f:(fun db -> FrogRun.db_add db t);
                 Lwt.return t
               | _  -> assert false
               (* If this happens, it means there is a hash collision
                  somewhere... *)
             end)
             set
         in
         Lwt.return (prover, Analyze.of_list l))
      provers
  in
  let%lwt () = Lwt_list.iter_p (fun (_,r) -> on_done r) res in
  Lwt.return res

