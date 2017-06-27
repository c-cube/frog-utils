
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Tests} *)

open Result
open Frog
open Lwt.Infix

module E = Misc.LwtErr

type dir = {
  directory : string;
  pattern : string;
  expect : ProblemSet.expect;
}

type config = {
  j : int;                       (* number of concurrent processes *)
  memory : int;                  (* memory limit for each problem *)
  timeout : int;                 (* timeout for each problem *)
  provers : Prover.t list;
  problems : dir list;
}

let expect_of_config config = function
  | None -> Ok ProblemSet.Auto
  | Some s ->
    let open Misc.Err in
    begin match Misc.Str.split ~by:':' s with
      | "program", p ->
        ProverSet.find_config config p >|= fun p -> ProblemSet.Program p
      | _ -> Ok (ProblemSet.Res (Res.of_string s))
      | exception Not_found -> Ok (ProblemSet.Res (Res.of_string s))
    end

let mk_config ?(profile="test") config dirs =
  let getter =
    let open Config in
    let tbl = table profile in
    (try_tables [tbl; top] @@ int "parallelism" <|> pure 1) >>= fun j ->
    (try_tables [tbl; top] @@ int "timeout" <|> pure 5) >>= fun timeout ->
    (try_tables [tbl; top] @@ int "memory" <|> pure 1000) >>= fun memory ->
    let problem_pat =
      try_tables [tbl; top] @@ string "problems"
    in
    let default_expect =
      (try_tables [tbl; top] @@ string "default_expect" >|= fun x-> Some x)
      <|> pure None
    in
    begin match dirs with
      | [] -> try_tables [tbl; top] @@ string_list ~default:[] "dir"
      | _ -> pure dirs
    end >>= fun l ->
    map_l
      (fun dir_name ->
         let dir_tbl = tbl |>> table dir_name in
         begin
         (dir_tbl |>> string "directory" <|> pure dir_name) >>= fun dir ->
         (dir_tbl |>> string "problems" <|> problem_pat) >>= fun pat ->
         ( (( some @@ try_tables [dir_tbl; tbl; top] @@ string "expect")
            <|> default_expect)
           >>= fun e -> (expect_of_config config e |> pure_or_error) )
         >|= fun expect ->
         { directory = dir; pattern = pat; expect = expect; }
         end |> add_ctxf "read config for directory `%s`" dir_name)
      l
    >>= fun problems ->
    ((try_tables [tbl; top] @@ string_list "provers") |> add_ctxf "get provers")
     >>= fun provers ->
    map_l
      (fun p -> ProverSet.find_config config p |> pure_or_error)
      provers
    >>= fun provers ->
    return { j; timeout; memory; provers; problems; }
  in
  Config.get config getter

let config_of_file ?profile file =
  Lwt_log.ign_debug_f "parse config file `%s`..." file;
  let open Misc.Err in
  Config.parse_file file >>= fun c ->
  mk_config ?profile c []

(* run one particular test *)
let run_pb_ ~config prover pb =
  Lwt_log.ign_debug_f "running %-15s/%-30s..."
    (Filename.basename prover.Prover.binary) pb.Problem.name;
  (* spawn process *)
  let%lwt result = Run.run_prover
      ~timeout:config.timeout
      ~memory:config.memory
      ~prover ~pb ()
  in
  Lwt_log.ign_debug_f "output for %s/%s: `%s`, `%s`, errcode %d"
    prover.Prover.binary pb.Problem.name
    result.Event.raw.Event.stdout
    result.Event.raw.Event.stderr
    result.Event.raw.Event.errcode;
  Lwt.return result

let run_pb ?(caching=true) ?limit ~config prover pb : _ E.t =
  let module V = Maki.Value in
  Maki.call
    ?limit
    ~bypass:(not caching)
    ~lifetime:(`KeepFor Maki.Time.(days 2))
    ~deps:[V.pack V.int config.timeout;
           V.pack V.int config.memory;
           V.pack Maki_wrapper.prover prover;
           V.pack Maki_wrapper.problem pb]
    ~op:Run.maki_result
    ~name:"frogtest.run_pb"
    (fun () -> run_pb_ ~config prover pb)
  |> E.of_exn

let nop_ _ = Lwt.return_unit

let print_result (res:Test.result): unit =
  let module F = Misc.Fmt in
  let p_res = Event.analyze_p res in
  let pp_res out () =
    let str, c = match Problem.compare_res res.Event.problem p_res with
      | `Same -> "ok", `Green
      | `Improvement -> "ok (improved)", `Blue
      | `Disappoint -> "disappoint", `Cyan
      | `Error -> "error", `Yellow
      | `Mismatch -> "bad", `Red
    in
    Format.fprintf out "%a" (F.in_bold_color c Format.pp_print_string) str
  in
  let prover = res.Event.program in
  let prover_name = Filename.basename prover.Prover.name in
  let pb_name = res.Event.problem.Problem.name in
  Lwt_log.ign_debug_f "result for `%s` with %s: %s (%.1fs)"
    prover_name pb_name (Res.to_string p_res) res.Event.raw.Event.rtime;
  Format.printf "%-20s%-50s %a (%.1fs)@." prover_name (pb_name ^ " :")
    pp_res () res.Event.raw.Event.rtime;
  ()

let run ?(on_solve = nop_) ?(caching=true) config =
  let open E.Infix in
  let limit = Maki.Limit.create config.j in
  E.map_p (fun dir ->
      let expect = dir.expect in
      let%lwt pbs = ProblemSet.of_dir dir.directory
          ~filter:(Re.execp (Re_posix.compile_pat dir.pattern)) in
      E.map_p
        (fun pb_path ->
           (* transform into problem *)
           let%lwt pb =
             Maki.Limit.acquire limit
               (fun () ->
                  let find_expect = ProblemSet.find_expect ~expect in
                  ProblemSet.make ~find_expect pb_path)
             |> Misc.LwtErr.to_exn
           in
           (* run provers *)
           E.map_p (fun prover ->
               run_pb ~caching ~limit ~config prover pb >>= fun result ->
                let%lwt () = on_solve result in (* callback *)
                E.return ()
                |> E.add_ctxf "running `%a` on %a"
                  Prover.pp_name prover Problem.print pb
             ) config.provers
        ) pbs
    ) config.problems >>= fun _ -> E.return ()

module Plot_res = struct
  type data =
    | Unsat_time
    | Sat_time
    | Both_time

  type legend =
    | Prover

  type drawer =
    | Simple of bool (* should we sort the list ? *)
    | Cumul of bool * int * int (* sort, filter, count *)

  type params = {
    graph : Plot.graph_config;
    data : data;
    legend : legend;
    drawer : drawer;
    out_file : string;
    out_format : string;
  }

  (*
  let draw params (r:Test.top_result): Plot.drawer =
    let lazy map = r.Test.analyze in
    let datas =
      Prover.Map_name.to_list map
      |> List.map
        (fun (prover,analyze) ->
           let name = match params.legend with
             | Prover -> Prover.name prover
           and points =
             T.MStr.to_list analyze.T.Analyze.raw
             |> Misc.List.filter_map
               (fun (_file,r) ->
                  let res = Event.analyze_p r in
                  let ok = match res, params.data with
                    | Res.Unsat, (Unsat_time | Both_time) -> true
                    | Res.Sat, (Sat_time | Both_time) -> true
                    | _ -> false
                  in
                  if ok then Some r.Event.raw.Event.rtime else None)
           in
           points, name
        )
    in
    let single_drawer = match params.drawer with
      | Simple sort -> Plot.float_list ~sort
      | Cumul (sort, filter, count) -> Plot.float_sum ~sort ~filter ~count
    in
    Plot.list @@ List.map single_drawer datas

  let draw_file params r =
    let d = draw params r in
    Plot.draw_on_graph params.graph ~fmt:params.out_format
      ~file:params.out_file d
  *)
end
