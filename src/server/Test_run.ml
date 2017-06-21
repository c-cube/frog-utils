
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
  | "" -> ProblemSet.Auto
  | s ->
    begin match Misc.Str.split ~by:':' s with
      | "program", p -> ProblemSet.Program (ProverSet.find_config config p)
      | _ -> ProblemSet.Res (Res.of_string s)
      | exception Not_found -> ProblemSet.Res (Res.of_string s)
    end

let mk_config config dirs =
  try
    let c = Config.get_table config "test" in
    let j = Config.get_int ~default:1 c "parallelism" in
    let timeout = Config.get_int ~default:5 c "timeout" in
    let memory = Config.get_int ~default:1000 c "memory" in
    let problem_pat = Config.get_string ~default:"" c "problems" in
    let default_expect = Config.get_string ~default:"" c "default_expect" in
    let l =
      match dirs with
      | [] -> Config.get_string_list ~default:[] c "dir"
      | _ -> dirs
    in
    let problems = List.map (fun s ->
        match Config.get_table c s with
        | t ->
          let dir = Config.get_string t "directory" in
          let pat = Config.get_string ~default:problem_pat t "problems" in
          let expect = expect_of_config config
              (Config.get_string ~default:default_expect t "expect") in
          { directory = dir; pattern = pat; expect = expect; }
        | exception (Config.FieldNotFound _ | TomlTypes.Table.Key.Bad_key _) ->
          { directory = s; pattern = problem_pat;
            expect = expect_of_config config default_expect; }
      ) l in
    let provers = Config.get_string_list c "provers" in
    let provers = List.map (ProverSet.find_config config) provers in
    Misc.Err.return { j; timeout; memory; provers; problems; }
  with
  | Config.Error e -> Misc.Err.fail e
  | e -> Misc.Err.fail (Printexc.to_string e)

let config_of_file file =
  Lwt_log.ign_debug_f "parse config file `%s`..." file;
  try
    let main = Config.parse_files [file] Config.empty in
    mk_config main []
  with
  | Config.Error e ->
    Misc.Err.fail e
  | e -> Misc.Err.fail (Printexc.to_string e)

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
