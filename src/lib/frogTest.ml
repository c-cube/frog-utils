
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

module Prover = FrogProver
module E = FrogMisc.Err
module W = FrogWeb

module Res = FrogRes
module Problem = FrogProblem
module ProblemSet = FrogProblemSet

module Results = FrogMap

type 'a or_error = 'a E.t
type 'a printer = Format.formatter -> 'a -> unit
type html = FrogWeb.html
type uri = FrogWeb.uri

let fpf = Format.fprintf
let spf = Format.asprintf

let assoc_or def x l =
  try List.assoc x l
  with Not_found -> def

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
  let%lwt result = FrogCmd.run_proc
    ~timeout:config.Config.timeout
    ~memory:config.Config.memory
    ~prover:config.Config.prover
    ~pb
    ()
  in
  Lwt_log.ign_debug_f "output for %s: `%s`, `%s`, errcode %d"
    pb.Problem.name result.FrogMap.stdout result.FrogMap.stderr result.FrogMap.errcode;
  (* parse its output *)
  let res = extract_res_ ~prover:config.Config.prover result.FrogMap.stdout result.FrogMap.errcode in
  Lwt.return { result with FrogMap.res }

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
  let%lwt () = on_done res in
  Lwt.return res

