
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

type 'a or_error = [`Ok of 'a | `Error of string]
type 'a printer = Format.formatter -> 'a -> unit
type html = FrogWeb.html
type uri = FrogWeb.uri

module MStr = Map.Make(String)
module Prover = FrogProver
module W = FrogWeb

let fpf = Format.fprintf
let spf = Format.asprintf

let assoc_or def x l =
  try List.assoc x l
  with Not_found -> def


(** {2 Result on a single problem} *)
module Res = struct
  [@@@warning "-39"]
  type t =
    | Sat
    | Unsat
    | Unknown
    | Error
    [@@deriving yojson]
  [@@@warning "+39"]

  let to_string = function
    | Sat -> "sat"
    | Unsat -> "unsat"
    | Unknown -> "unknown"
    | Error -> "error"

  let print out s = Format.pp_print_string out (to_string s)

  let compare a b = match a, b with
    | Unsat, Unsat
    | Sat, Sat
    | Unknown, Unknown
    | Error, Error -> `Same
    | Unknown, (Sat | Unsat) -> `RightBetter
    | (Sat | Unsat), Unknown -> `LeftBetter
    | (Unsat | Error), Sat
    | (Sat | Error), Unsat
    | Error, Unknown
    | (Sat | Unknown | Unsat), Error ->
        `Mismatch

  let maki : t Maki.Value.ops = Maki_yojson.make_err ~to_yojson ~of_yojson "result"

  let to_html s =
    let module H = FrogWeb.Html in
    let color = match s with
      | Unsat | Sat -> "darkgreen"
      | Unknown -> "orange"
      | Error -> "red"
    in
    H.string (to_string s)
    |> H.div ~cls:"result" ~attrs:["style", "color:"^color]
end

module Problem = struct
  [@@@warning "-39"]
  type t = {
    name: string;  (* filename *)
    expected: Res.t; (* result expected *)
  } [@@deriving yojson]
  [@@@warning "+39"]

  let same_name t1 t2 = t1.name = t2.name
  let compare_name t1 t2 = Pervasives.compare t1.name t2.name

  (* regex + mark *)
  let m_unsat_, unsat_ = Re.(str "unsat" |> no_case |> mark)
  let m_sat_, sat_ = Re.(str "sat" |> no_case |> mark)
  let m_unknown_, unknown_ = Re.(str "unknown" |> no_case |> mark)
  let m_error_, error_ = Re.(alt [str "error"; str "fail"] |> no_case |> mark)

  (* "^ #expect: (unsat|sat|unknown|error)", basically *)
  let re_expect_ = Re.(seq
    [ alt (List.map no_case [str "expect:"; str "expected:"]) ; rep blank;
      alt [unsat_; sat_; unknown_; error_] ]
    |> compile
  )

  (* what is expected? *)
  let find_expected_ ~file =
    let%lwt content = FrogMisc.File.with_in ~file FrogMisc.File.read_all in
    let g = Re.exec re_expect_ content in
    if Re.marked g m_unsat_ then Lwt.return Res.Unsat
    else if Re.marked g m_sat_ then Lwt.return Res.Sat
    else if Re.marked g m_unknown_ then Lwt.return Res.Unknown
    else if Re.marked g m_error_ then Lwt.return Res.Error
    else Lwt.fail Not_found

  let make ~file =
    try%lwt
      Lwt_log.ign_debug_f "convert `%s` into problem..." file;
      let%lwt res = find_expected_ ~file in
      let pb = {
        name=file;
        expected=res;
      } in
      Lwt.return (`Ok pb)
    with e ->
      Lwt.return
        (`Error (spf "could not find expected res for %s: %s"
                file (Printexc.to_string e)))

  let compare_res pb res =
    match pb.expected, res with
    | Res.Unsat, Res.Unsat
    | Res.Sat, Res.Sat
    | Res.Unknown, Res.Unknown
    | Res.Error, Res.Error -> `Same
    | (Res.Sat | Res.Unsat | Res.Error), Res.Unknown -> `Disappoint
    | (Res.Unsat | Res.Error), Res.Sat
    | (Res.Sat | Res.Error), Res.Unsat
    | (Res.Sat | Res.Unknown | Res.Unsat), Res.Error ->
        `Mismatch
    | Res.Unknown, (Res.Sat | Res.Unsat) ->
        `Improvement

  let print out p =
    fpf out "@[<h>%s (expect: %a)@]" p.name Res.print p.expected

  let to_string p = FrogMisc.Fmt.to_string print p

  let maki =
    let module V = Maki.Value in
    V.map ~descr:"problem"
      (fun p -> p.name, p.expected)
      (fun (name, expected) -> {name;expected})
      (V.pair V.file Res.maki)

  let to_html_name p = W.Html.string p.name
  let to_html_full p = W.Html.string (to_string p)

  let hash p : string =
    Sha1.string p.name |> Sha1.to_hex

  let k_uri = W.HMap.Key.create ("uri_of_problem", fun _ -> Sexplib.Sexp.Atom "")
  let k_add = W.HMap.Key.create ("add_problem", fun _ -> Sexplib.Sexp.Atom "")

  let add_server s =
    (* md5(problem.name) -> problem *)
    let uri_of_pb pb =
      Uri.make ~path:("/problem/" ^ hash pb) ()
    and add_pb pb =
      W.DB.add_json ~f:to_yojson (W.Server.db s) ("problem-" ^ hash pb) pb
    in
    let handler req =
      let open Opium.Std in
      let h = param req "hash" in
      match W.DB.get_json ~f:of_yojson (W.Server.db s) ("problem-" ^ h) with
        | `Ok pb ->
          (* read the problem itself *)
          Lwt_io.with_file ~mode:Lwt_io.input pb.name
            FrogMisc.File.read_all
          >>= fun content ->
          W.Html.list
            [ to_html_full pb
            ; W.pre (W.Html.string content)
            ]
          |> W.Server.return_html ~title:"problem"
        | `Error msg ->
          let code = Cohttp.Code.status_of_code 404 in
          let h =
            W.Html.string (Printf.sprintf "could not find problem %s: %s" h msg) in
          W.Server.return_html ~code h
    in
    W.Server.set s k_uri uri_of_pb;
    W.Server.set s k_add add_pb;
    W.Server.add_route s "/problem/:hash" handler
end

module ProblemSet = struct
  type t = Problem.t list

  let make l =
    let pool = Lwt_pool.create 30 (fun () -> Lwt.return_unit) in
    let%lwt l =
      Lwt_list.map_p
        (fun file ->
          Lwt_pool.use pool
            (fun () -> Problem.make ~file))
        l
    in
    let l = FrogMisc.Err.seq_list l in
    (* sort by alphabetic order *)
    let l = FrogMisc.Err.(l >|= List.sort Problem.compare_name) in
    Lwt.return l

  let size = List.length

  let of_dir ?filter:(p=fun _ -> true) d =
    let l = FrogMisc.File.walk d in
    let l = FrogMisc.List.filter_map
      (fun (kind,f) -> match kind with
        | `File when p f -> Some f
        | _ -> None
      ) l
    in
    make l

  let print out set =
    fpf out "@[<hv>%a@]" (Format.pp_print_list Problem.print) set

  let maki = Maki.Value.set Problem.maki

  let to_html uri_of_pb l =
    let module H = FrogWeb.Html in
    let f pb = H.a ~href:(uri_of_pb pb) (Problem.to_html_name pb) in
    H.div ~attrs:["class", "problem_set"]
      (H.list (List.map f l))
end

module Config = struct
  [@@@warning "-39"]
  type t = {
    j: int; (* number of concurrent processes *)
    timeout: int; (* timeout for each problem *)
    memory: int;
    problem_pat: string; (* regex for problems *)
    prover: Prover.t;
  } [@@deriving yojson]
  [@@@warning "+39"]

  let maki : t Maki.Value.ops =
    let module V = Maki.Value in
    let json =  Maki_yojson.make_err ~of_yojson ~to_yojson "config_json" in
    V.map ~descr:"config"
      (fun t -> t, t.prover.Prover.binary)
      (fun (t,_) -> t)
      (V.pair json V.program)

  let make ?(j=1) ?(timeout=5) ?(memory=1000) ~pat ~prover () =
    { j; timeout; memory; prover; problem_pat=pat; }

  let update ?j ?timeout ?memory c =
    let module O = FrogMisc.Opt in
    let j = O.get c.j j in
    let timeout = O.get c.timeout timeout in
    let memory = O.get c.memory memory in
    { c with j; timeout; memory; }

  let of_file file =
    let module E = FrogMisc.Err in
    let module C = FrogConfig in
    Lwt_log.ign_debug_f "parse config file `%s`..." file;
    try
      let c = C.parse_files [file] C.empty in
      let j = C.get_int ~default:1 c "parallelism" in
      let timeout = C.get_int ~default:5 c "timeout" in
      let memory = C.get_int ~default:1000 c "memory" in
      let prover = C.get_string c "prover" in
      let prover = Prover.build_from_config c prover in
      let problem_pat = C.get_string c "problems" in
      E.return { j; timeout; memory; prover; problem_pat; }
    with
    | C.Error e -> E.fail e
    | Not_found -> E.fail ("invalid config file: " ^ file)

  let to_html uri_of_prover c =
    let module H = W.Html in
    let module R = W.Record in
    R.start
    |> R.add_int "j" c.j
    |> R.add_int "timeout" c.timeout
    |> R.add_int "memory" c.memory
    |> R.add_string "problems pattern" c.problem_pat
    |> R.add "prover"
      (H.a ~href:(uri_of_prover c.prover) (Prover.to_html_name c.prover))
    |> R.close

  let add_server s c =
    let uri_of_prover = W.Server.get s Prover.k_uri in
    let handle _ =
      W.Server.return_html (to_html uri_of_prover c)
    in
    W.Server.add_route s ~descr:"configuration" "/config" handle;
    ()
end

module Results = struct
  type raw_result = {
    problem: Problem.t;
    res: Res.t;
    stdout: string;
    stderr: string;
    errcode: int;
  } [@@deriving yojson]

  type raw = raw_result MStr.t

  let raw_of_list l =
    List.fold_left
      (fun acc r -> MStr.add r.problem.Problem.name r acc)
      MStr.empty l

  let raw_to_yojson r : Yojson.Safe.json =
    let l = MStr.fold
      (fun _ r acc ->
        let j = raw_result_to_yojson r in
        j :: acc)
      r []
    in
    `List l

  (* map raw_result to a unique hex string (hash) *)
  let hash_raw_res (r:raw_result) : string =
    let s = raw_result_to_yojson r |> Yojson.Safe.to_string in
    Sha1.string s |> Sha1.to_hex

  module E = FrogMisc.Err

  let raw_of_yojson (j:Yojson.Safe.json) : raw or_error =
    let open E in
    let get_list_ = function
      | `List l -> E.return l
      | _ -> E.fail "expected list"
    in
    get_list_ j >|= List.map raw_result_of_yojson >>= E.seq_list >|= raw_of_list

  type stat = {
    unsat: int;
    sat: int;
    errors: int;
    unknown: int;
  }

  let stat_empty = {unsat=0; sat=0; errors=0; unknown=0; }

  let add_sat_ s = {s with sat=s.sat+1}
  let add_unsat_ s = {s with unsat=s.unsat+1}
  let add_unknown_ s = {s with unknown=s.unknown+1}
  let add_error_ s = {s with errors=s.errors+1}

  let pp_stat out s =
    fpf out "{@[<hv>unsat: %d,@ sat: %d,@ errors: %d,@ unknown: %d,@ total: %d@]}"
      s.unsat s.sat s.errors s.unknown (s.unsat + s.sat + s.errors + s.unknown)

  type t = {
    raw: raw;
    stat: stat;
    ok: raw_result list;
    disappoint: raw_result list;
    bad: raw_result list;
  }

  let pp_raw_res_ out r =
    fpf out "@[<h>problem %s (expected: %a, result: %a)@]"
      r.problem.Problem.name Res.print r.problem.Problem.expected Res.print r.res

  let pp_list_ p = Format.pp_print_list p

  (* build statistics and list of mismatch from raw results *)
  let analyse_ raw =
    let module M = OLinq.AdaptMap(MStr) in
    let l =
      M.of_map raw
      |> OLinq.map snd
      |> OLinq.group_by
        (fun r -> Problem.compare_res r.problem r.res)
      |> OLinq.run_list ?limit:None
    in
    let ok = assoc_or [] `Improvement l in
    let bad = assoc_or [] `Mismatch l in
    let disappoint = assoc_or [] `Disappoint l in
    (* stats *)
    let stat = ref stat_empty in
    let add_res res =
      stat := (match res with
        | Res.Unsat -> add_unsat_ | Res.Sat -> add_sat_
        | Res.Unknown -> add_unknown_ | Res.Error -> add_error_
      ) !stat
    in
    MStr.iter (fun _ r -> add_res r.res) raw;
    ok, bad, disappoint, !stat

  let add_raw raw r =
    MStr.add r.problem.Problem.name r raw

  let make raw =
    let ok, bad, disappoint, stat = analyse_ raw in
    { raw; stat; ok; disappoint; bad; }

  let of_yojson j = E.(raw_of_yojson j >|= make)
  let to_yojson t = raw_to_yojson t.raw

  let of_list l =
    let raw = raw_of_list l in
    make raw

  let of_file ~file =
    try
      let json = Yojson.Safe.from_file file in
      of_yojson json
    with e -> E.fail (Printexc.to_string e)

  let to_file t ~file = Yojson.Safe.to_file file (to_yojson t)

  let is_ok r = r.bad = []
  let num_failed r = List.length r.bad

  let print out r =
    let pp_l out = fpf out "[@[<hv>%a@]]" (pp_list_ pp_raw_res_) in
    fpf out
      "@[<hv2>results: {@,stat:%a,@ %-15s: %a,@ %-15s: %a,@ %-15s: %a@]@,}"
      pp_stat r.stat
      "ok" pp_l r.ok
      "disappoint" pp_l r.disappoint
      "bad" pp_l r.bad

  let maki_raw_res =
    Maki_yojson.make_err "raw_res"
      ~to_yojson:raw_result_to_yojson
      ~of_yojson:raw_result_of_yojson

  (* FIXME: use result, not poly variant *)
  let maki =
    Maki_yojson.make_err "results"
      ~to_yojson
      ~of_yojson

  module H = W.Html
  module R = W.Record

  (* display the raw result *)
  let to_html_raw_result uri_of_problem r =
    R.start
    |> R.add "problem"
      (H.a ~href:(uri_of_problem r.problem) (Problem.to_html_name r.problem))
    |> R.add "result" (Res.to_html r.res)
    |> R.add_int "errcode" r.errcode
    |> R.add "stdout" (W.pre (H.string r.stdout))
    |> R.add "stderr" (W.pre (H.string r.stderr))
    |> R.close

  let to_html_stats s =
    R.start
    |> R.add_int "unsat" s.unsat
    |> R.add_int "sat" s.sat
    |> R.add_int "errors" s.errors
    |> R.add_int "unknown" s.unknown
    |> R.close

  let to_html_raw_result_l uri_of_problem uri_of_raw_res r =
    [ H.a ~href:(uri_of_problem r.problem) (Problem.to_html_name r.problem)
    ; H.a ~href:(uri_of_raw_res r) (Res.to_html r.res)
    ]

  let to_html_raw uri_of_problem uri_of_raw_res r =
    let l = MStr.fold (fun _ r acc -> `Row r::acc) r [] in
    H.Create.table ~flags:[H.Create.Tags.Headings_fst_row]
      ~row:(function
        | `Head -> [H.string "problem"; H.string "result"]
        | `Row r -> to_html_raw_result_l uri_of_problem uri_of_raw_res r)
      (`Head :: l)

  (* TODO: print tables good/disappoint/bad, then, lower, print raw *)
  let to_html uri_of_problem uri_of_raw_res t =
    let lst_raw_res ?cls l =
      H.Create.table l ~flags:[]
        ~row:(to_html_raw_result_l uri_of_problem uri_of_raw_res)
      |> H.div ?cls
    in
    R.start
    |> R.add "stats" (to_html_stats t.stat)
    |> R.add "ok" (lst_raw_res t.ok)
    |> R.add "disappoint" (lst_raw_res t.disappoint)
    |> R.add "bad" (lst_raw_res t.bad)
    |> R.add "raw" (to_html_raw uri_of_problem uri_of_raw_res t.raw)
    |> R.close

  let k_add = W.HMap.Key.create ("add_result", fun _ -> Sexplib.Sexp.Atom "")
  let k_set = W.HMap.Key.create ("set_results", fun _ -> Sexplib.Sexp.Atom "")

  let add_server s =
    let open Opium.Std in
    let cur = ref (`Partial MStr.empty) in
    let db = W.Server.db s in
    let uri_of_problem = W.Server.get s Problem.k_uri in
    (* find raw result *)
    let handle_res req =
      let h = param req "hash" in
      begin match W.DB.get_json ~f:raw_result_of_yojson db ("raw_result-"^h) with
        | `Ok r ->
          W.Server.return_html (to_html_raw_result uri_of_problem r)
        | `Error msg ->
          let code = Cohttp.Code.status_of_code 404 in
          let h = H.string ("unknown result: " ^ msg) in
          W.Server.return_html ~code h
      end
    (* display all the results *)
    and handle_main _ =
      let uri_of_raw_res r = Uri.make ~path:("/result/" ^ hash_raw_res r) () in
      let is_done = match !cur with `Partial _ -> false | `Done _ -> true in
      let h =
        H.list
          [ H.i (H.string ("done: " ^ string_of_bool is_done))
          ; begin match !cur with
            | `Done c -> to_html uri_of_problem uri_of_raw_res c
            | `Partial c -> to_html_raw uri_of_problem uri_of_raw_res c
            end
          ]
      in
      W.Server.return_html ~title:"results" h
    and on_add r =
      begin match !cur with
        | `Done _ -> assert false
        | `Partial c -> cur := `Partial (add_raw c r)
      end;
      let h = hash_raw_res r in
      W.DB.add_json ~f:raw_result_to_yojson db ("raw_result-" ^ h) r;
    and on_done r = match !cur with
      | `Partial _ -> cur := `Done r
      | `Done _ -> assert false
    in
    W.Server.set s k_add on_add;
    W.Server.set s k_set on_done;
    W.Server.add_route s ~descr:"current results" "/results" handle_main;
    W.Server.add_route s "/result/:hash" handle_res;
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
  let compare (a:Results.raw) b : t =
    let open Results in
    let module M = OLinq.AdaptMap(MStr) in
    let a = M.of_map a |> OLinq.map snd in
    let b = M.of_map b |> OLinq.map snd in
    let j =
      OLinq.join ~eq:Problem.same_name
        (fun r -> r.problem) (fun r -> r.problem) a b
        ~merge:(fun pb r1 r2 ->
          assert (r1.problem.Problem.name = r2.problem.Problem.name);
          Some (pb, r1.res, r2.res))
      |> OLinq.group_by (fun (_,res1,res2) -> Res.compare res1 res2)
      |> OLinq.run_list
    in
    let improved = assoc_or [] `RightBetter j in
    let regressed = assoc_or [] `LeftBetter j in
    let mismatch = assoc_or [] `Mismatch j in
    let same = assoc_or [] `Same j |> List.rev_map (fun (pb,r,_) -> pb,r) in
    let disappeared =
      OLinq.diff ~cmp:(fun r1 r2 -> Problem.compare_name r1.problem r2.problem) a b
      |> OLinq.map (fun r -> r.problem, r.res)
      |> OLinq.run_list
    and appeared =
      OLinq.diff ~cmp:(fun r1 r2 -> Problem.compare_name r1.problem r2.problem) b a
      |> OLinq.map (fun r -> r.problem, r.res)
      |> OLinq.run_list
    in
    { appeared; disappeared; mismatch; same; regressed; improved; }

  let fpf = Format.fprintf
  let pp_list_ p = Format.pp_print_list p
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

let extract_res_ ~prover stdout errcode =
  (* find if [re: re option] is present in [stdout] *)
  let find_opt_ re = match re with
    | None -> false
    | Some re -> Re.execp (Re_posix.compile_pat re) stdout
  in
  if errcode <> 0 then Res.Error
  else if find_opt_ prover.Prover.sat then Res.Sat
  else if find_opt_ prover.Prover.unsat then Res.Unsat
  else if (find_opt_ prover.Prover.timeout || find_opt_ prover.Prover.unknown)
    then Res.Unknown
    else Res.Error

(* run one particular test *)
let run_pb_ ~config pb =
  Lwt_log.ign_debug_f "running %-30s..." pb.Problem.name;
  (* spawn process *)
  let%lwt (out,err,errcode) = Prover.run_proc
    ~timeout:config.Config.timeout
    ~memory:config.Config.memory
    ~prover:config.Config.prover
    ~file:pb.Problem.name
    ()
  in
  Lwt_log.ign_debug_f "output for %s: `%s`, `%s`, errcode %d"
    pb.Problem.name out err errcode;
  (* parse its output *)
  let res = extract_res_ ~prover:config.Config.prover out errcode in
  let raw_res = {Results. res; problem=pb; stdout=out; stderr=err; errcode} in
  Lwt.return raw_res

let run_pb ?(caching=true) ?limit ~config pb =
  let module V = Maki.Value in
  Maki.call_exn
    ?limit
    ~bypass:(not caching)
    ~lifetime:(`KeepFor Maki.Time.(days 2))
    ~deps:[V.pack Config.maki config; V.pack Problem.maki pb]
    ~op:Results.maki_raw_res
    ~name:"frogtest.run_pb"
    (fun () -> run_pb_ ~config pb)

let nop_ _ = Lwt.return_unit
let nop2_ _ _ = Lwt.return_unit

let run ?(on_solve = nop2_) ?(on_done = nop_)
    ?(caching=true) ?j ?timeout ?memory ?server ~config set
  =
  let config = Config.update ?j ?timeout ?memory config in
  let limit = Maki.Limit.create config.Config.j in
  FrogMisc.Opt.iter server
    ~f:(fun s ->
      Prover.add_server s;
      Problem.add_server s;
      Results.add_server s;
      Config.add_server s config;
      W.Server.get s Prover.k_add config.Config.prover;
    );
  let%lwt raw =
    Lwt_list.map_p
      (fun pb ->
         let%lwt raw_res = run_pb ~caching ~limit ~config pb in
         let%lwt () = on_solve pb raw_res.Results.res in
         (* add result to server? *)
         FrogMisc.Opt.iter server
           ~f:(fun s ->
             W.Server.get s Problem.k_add pb;
             W.Server.get s Results.k_add raw_res);
         Lwt.return raw_res)
      set
  in
  let res = Results.of_list raw in
  FrogMisc.Opt.iter server
    ~f:(fun s -> W.Server.get s Results.k_set res);
  let%lwt () = on_done res in
  Lwt.return res
