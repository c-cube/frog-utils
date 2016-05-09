
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Manage TPTP provers} *)

module StrMap = Map.Make(String)
module St = FrogMapState
module Prover = FrogProver
module Conf = FrogConfig
module PB = FrogPrintBox
module Opt = FrogMisc.Opt

(* Misc function *)
let debug fmt = Lwt_log.ign_debug_f fmt

let compile_re ~msg re =
  try
    Some (Re.compile (Re.no_case (Re_posix.re re)))
  with e ->
    Printf.eprintf "could not compile regex %s: %s"
      msg (Printexc.to_string e);
    None

let execp_re_maybe maybe_re s = match maybe_re with
  | None -> false
  | Some re -> Re.execp re s

(* obtain the full list of problems/results deal with in this file *)
let extract_file file =
  St.fold_state
    (fun (job,map) res ->
      job, StrMap.add res.St.res_arg res map
    ) (fun job -> job, StrMap.empty) file
    |> Lwt_main.run

(* Single summary analysis *)
let print_single_summary prover s =
  let open FrogTPTP in
  let num_sat = StrMap.cardinal s.set_sat in
  let num_unsat = StrMap.cardinal s.set_unsat in
  let percent_solved = (float (num_sat + num_unsat) *. 100. /. float s.num_all) in
  Printf.printf "prover %s: %d sat, %d unsat, %d total (%.0f%% solved), %d errors\n"
    prover num_sat num_unsat s.num_all percent_solved s.num_error;
  Printf.printf "solved time : %.2fs (real), %.2f (user), %.2f (system)\n"
    s.solved_time.real s.solved_time.user s.solved_time.system;
  Printf.printf "total run time : %.2fs (real), %.2f (user), %.2f (system)\n"
    s.run_time.real s.run_time.user s.run_time.system;
  ()

let all_proved s =
  StrMap.merge (fun _ _ _ -> Some ()) s.FrogTPTP.set_unsat s.FrogTPTP.set_sat

(* input: map prover -> summary
  output: map problem -> unit, containing every problem solved by at
      least one prover *)
let all_proved_map map =
  try
    let min_p, s = StrMap.min_binding map in
    let acc = all_proved s in
    let others = StrMap.remove min_p map in
    StrMap.fold
      (fun _ s acc ->
        acc
        |> StrMap.merge (fun _ _ _ -> Some ()) s.FrogTPTP.set_unsat
        |> StrMap.merge (fun _ _ _ -> Some ()) s.FrogTPTP.set_sat
      ) others acc
  with Not_found ->
    StrMap.empty

let map_diff m1 m2 =
  StrMap.merge
    (fun _ x1 x2 -> match x1, x2 with
      | Some x, None -> Some x
      | _ -> None)
    m1 m2

let analyse ~config params l = match l with
  | [] -> assert false
  | [p, file] ->
      debug "analyse file %s, obtained from prover %s" file p;
      let s = FrogTPTP.analyse_single_file ~config p file in
      FrogTPTP.print_file_summary stdout s
  | _ ->
      debug "analyse %d files" (List.length l);
      let l = FrogTPTP.analyse_multiple params (FrogTPTP.parse_prover_list ~config l) in
      let box = FrogTPTP.box_of_ar l in
      PB.output stdout box;
      List.iter (FrogTPTP.print_ar_exclusive stdout) l;
      ()

type plot_data =
  | Unsat_Time

type plot_legend =
  | Prover
  | File

type plot_drawer =
  | Simple of bool (* should we sort the list ? *)
  | Cumul of bool * int * int (* sort, filter, count *)

type plot_params = {
  analyse : FrogTPTP.analyse_params;
  graph : FrogPlot.graph_config;
  data : plot_data;
  legend : plot_legend;
  drawer : plot_drawer;
  out_file : string;
  out_format : string;
}

(* Plot functions *)
let plot ~config params l =
  let items = FrogTPTP.parse_prover_list ~config l in
  let map = FrogTPTP.map_summaries params.analyse items in
  let datas =
    StrMap.fold (fun file s acc ->
      let name = match params.legend with
        | Prover -> s.FrogTPTP.prover
        | File -> Filename.basename file
      in
      let l = match params.data with
        | Unsat_Time ->
          StrMap.fold
            (fun _ time acc -> params.analyse.FrogTPTP.get_time time :: acc)
            s.FrogTPTP.set_unsat []
          |> List.rev
      in
      (l, name) :: acc) map []
  in
  let single_drawer = match params.drawer with
    | Simple sort -> FrogPlot.float_list ~sort
    | Cumul (sort, filter, count) -> FrogPlot.float_sum ~sort ~filter ~count
  in
  let drawer = FrogPlot.list @@ List.map single_drawer datas in
  FrogPlot.draw_on_graph
    params.graph
    ~fmt:params.out_format
    ~file:params.out_file
    drawer

(* print list of known provers *)
let list_provers ~config =
  let provers = Prover.of_config config in
  Printf.printf "provers:\n";
  StrMap.iter
    (fun name p -> Printf.printf "  %s: cmd=%s\n" name p.Prover.cmd)
    provers

(** {2 Run} *)

(* Argument parsing *)
let use_time =
  (function
    | "real" -> `Ok (fun { FrogTPTP.real; _ } -> real)
    | "user" -> `Ok (fun { FrogTPTP.user; _ } -> user)
    | "sys"  -> `Ok (fun { FrogTPTP.system; _ } -> system)
    | "cpu"  -> `Ok (fun { FrogTPTP.user; system; _ } -> user +. system)
    | _ -> `Error "Must be one of : 'real', 'user', 'sys', or 'cpu'"),
  (fun fmt _ -> Format.fprintf fmt "*abstract*")

let filter_pbs =
  (function
    | "all" -> `Ok (fun _ _ _ -> true)
    | "common" ->
      `Ok
        (fun map problem _ ->
           StrMap.for_all
             (fun _ s -> StrMap.mem problem s.FrogTPTP.set_unsat)
             map)
    | _ -> `Error "Must be one of : 'all', 'common'"),
  (fun fmt _ -> Format.fprintf fmt "*abstract*")

let to_cmd_arg l = Cmdliner.Arg.enum l, Cmdliner.Arg.doc_alts_enum l

let data_conv, data_help = to_cmd_arg [
    "unsat_time", Unsat_Time;
  ]

let legend_conv, legend_help = to_cmd_arg [
    "prover", Prover;
    "file", File;
  ]

(** {2 Main} *)

let some_if_pos_ i =
  if i>0 then Some i else None

let args_term =
  let open Cmdliner in
  let doc = "List of pairs of prover and corresponding output file to analyse. This should be a comma-separated
            list of pairs 'prover=file'" in
  Arg.(non_empty & pos 0 (list (pair ~sep:'=' string non_dir_file)) [] & info [] ~docv:"ARGS" ~doc)

let config_term =
  let open Cmdliner in
  let aux config debug =
    if debug then begin
      Lwt_log.add_rule "*" Lwt_log.Debug;
      Printexc.record_backtrace true
    end;
    try
      `Ok (FrogConfig.parse_files config FrogConfig.empty)
    with FrogConfig.Error msg ->
      `Error (false, msg)
  in
  let config =
    let doc = "Use the given config files. Defaults to '~/.frogtptp.toml'. If one or more config files is
               given, only those files are taken into account (instead of the default one)." in
    Arg.(value & opt_all non_dir_file [Conf.interpolate_home "$HOME/.frogtptp.toml"] & info ["c"; "config"] ~doc)
  in
  let debug =
    let doc = "Enable debug (verbose) output" in
    Arg.(value & flag & info ["d"; "debug"] ~doc)
  in
  Term.(ret (pure aux $ config $ debug))

let limit_term =
  let open Cmdliner in
  let aux memory timeout =
    { FrogTPTP.memory = memory; timeout = timeout;}
  in
  let memory =
    let doc = "Memory limit" in
    Arg.(value & opt (some int) None & info ["m"; "memory"] ~doc)
  in
  let timeout =
    let doc = "Time limit" in
    Arg.(value & opt (some int) None & info ["t"; "timeout"] ~doc)
  in
  Term.(pure aux $ memory $ timeout)

let analyze_params_term =
  let open Cmdliner in
  let aux get_time filter = { FrogTPTP.get_time; filter; } in
  let use_time =
    let doc = "Specify which time to use for comparing provers. Choices are :
               'real' (real time elasped between start and end of execution),
               'user' (user time, as defined by the 'time 'command),
               'sys' (system time as defined by the 'time' command),
               'cpu' (sum of user and system time)." in
    Arg.(value & opt use_time (fun { FrogTPTP.real; _} -> real) & info ["t"; "time"] ~doc)
  in
  let filter =
    let doc = "TODO" in
    Arg.(value & opt filter_pbs (fun _ _ _ -> true) & info ["filter"] ~doc)
  in
  Term.(pure aux $ use_time $ filter)

let drawer_term =
  let open Cmdliner in
  let aux cumul sort filter count =
    if cumul then Cumul (sort, filter, count) else Simple sort
  in
  let cumul =
    let doc = "Plots the cumulative sum of the data" in
    Arg.(value & opt bool true & info ["cumul"] ~doc)
  in
  let sort =
    let doc = "Should the data be sorted before being plotted" in
    Arg.(value & opt bool true & info ["sort"] ~doc)
  in
  let filter =
    let doc = "Plots one in every $(docv) data point
              (ignored if not in cumulative plotting)" in
    Arg.(value & opt int 3 & info ["pspace"] ~doc)
  in
  let count =
    let doc = "Plots the last $(docv) data point in any case" in
    Arg.(value & opt int 5 & info ["count"] ~doc)
  in
  Term.(pure aux $ cumul $ sort $ filter $ count)

let plot_params_term =
  let open Cmdliner in
  let aux analyse graph data legend drawer out_file out_format =
    { analyse; graph; data; legend; drawer; out_file; out_format }
  in
  let data =
    let doc = Format.sprintf "Decides which value to plot. $(docv) must be %s" data_help in
    Arg.(value & opt data_conv Unsat_Time & info ["data"] ~doc)
  in
  let legend =
    let doc = Format.sprintf
        "What legend to attach to each curve. $(docv) must be %s" legend_help
    in
    Arg.(value & opt legend_conv Prover & info ["legend"] ~doc)
  in
  let out_file =
    let doc = "Output file for the plot" in
    Arg.(required & opt (some string) None & info ["o"; "out"] ~doc)
  in
  let out_format =
    let doc = "Output format for the graph" in
    Arg.(value & opt string "PDF" & info ["format"] ~doc)
  in
  Term.(pure aux $ analyze_params_term $ FrogPlot.graph_args $ data $ legend $ drawer_term $ out_file $ out_format)

let analyze_term =
  let open Cmdliner in
  let aux config params l = analyse ~config params l
  in
  let doc = "Analyze the results of provers run previously" in
  let man = [
    `S "DESCRIPTION";
    `P "Analyse the prover's results and prints statistics about them.
        TODO: more detailed explication.";
  ] in
  Term.(pure aux $ config_term $ analyze_params_term $ args_term),
  Term.info ~man ~doc "analyse"

let run_term =
  let open Cmdliner in
  let aux config params cmd args =
    let timeout =
      match params.FrogTPTP.timeout with
      | Some res -> res
      | None ->
        begin match FrogConfig.get_int config "timeout" with
          | x -> x
          | exception Not_found ->
            failwith "A timeout is required (either on the command line or in the config file)"
        end
    in
    let memory =
      match params.FrogTPTP.memory with
      | Some res -> res
      | None ->
        begin match FrogConfig.get_int config "memory" with
          | x -> x
          | exception Not_found ->
            failwith "A memory limit is required (either on the command line or in the config file)"
        end
    in
    let prover = Prover.find_config config cmd in
    Prover.run_exec ~timeout ~memory ~prover ~file:(String.concat " " args) ()
  in
  let cmd =
    let doc = "Prover to be run" in
    Arg.(required & pos 0 (some string) None & info [] ~doc)
  in
  let args =
    let doc = "Arguments to be passed to the prover" in
    Arg.(value & pos_right 0 string [] & info [] ~doc)
  in
  let doc = "Run the prover on the given arguments" in
  let man = [
    `S "DESCRIPTION";
    `P "This tool allows to run provers with pre-set options and uniform options
        for time and memory limits. Provers to be run must be present in one of the
        configuration files specified with the options.";
  ] in
  Term.(pure aux $ config_term $ limit_term $ cmd $ args),
  Term.info ~man ~doc "run"

let list_term =
  let open Cmdliner in
  let aux config = list_provers ~config in
  let doc = "List the known provers,given the config files specified (see 'config' option)" in
  let man = [
    `S "DESCRIPTION";
    `P "Prints on stdout the list of known provers according to the configuration
        files specified with the options.";
    `P "The list conatins one line for each prover, and is of the form : '<name>: cmd=<cmd>'.";
  ] in
  Term.(pure aux $ config_term),
  Term.info ~man ~doc "list"

let plot_term =
  let open Cmdliner in
  let aux config params args = plot ~config params args in
  let doc = "Plot graphs of prover's statistics" in
  let man = [
    `S "DESCRIPTION";
    `P "This tools takes results files from runs of '$(b,frogmap)' and plots graphs
        about the prover's statistics.";
    `S "OPTIONS";
    `S FrogPlot.graph_section;
  ] in
  Term.(pure aux $ config_term $ plot_params_term $ args_term),
  Term.info ~man ~doc "plot"

let help_term =
  let open Cmdliner in
  let doc = "Offers various utilities to test automated theorem provers." in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,frogtptp) offers various utilities to simplify benchmarks of automated theorem provers.
        It includes a command to run proverrs with preset options, an analyser of benchmarks results,
        and an utility producing graphs of the provers' results (coming very soon).";
    `S "COMMANDS";
    `S "OPTIONS";
    `S "CONFIGURATION FILE";
    `P "Configuration files for $(b,frogtptp) follows the toml language. It should contain the following:
        default timeouts and memory limits, a list of known provers, and a section for each of these provers.";
    `P "This tool will only look for prover sections which are in the list of known provers, so if there is a section
        about a prover that is not in the list, it will simply be ignored.";
    `P "Each prover section must provide a '$(b,cmd)' parameter, which is the command to be called when running
        the prover. Two additionnal parameters may be specified: $(b,sat) and $(b,unsat), which are regular expression
        that will be used to decide the satisfiability status of the input problem according to the prover, by matching
        it against the stdout output of the prover. In the command, the following expressions will be substituted:";
    `I ("$(b,\\${timeout})", "will be substituted with the given timeout (in seconds).");
    `I ("$(b,\\${memory})", "will be substituted wiuth the given memory limit (in Mo).");
    `I ("$(b,\\${file})", "will be substituted with the the path of the input problem file.");
  ] in
  Term.(ret (pure (fun () -> `Help (`Pager, None)) $ pure ())),
  Term.info ~version:"dev" ~man ~doc "frogtptp"

let () =
  match Cmdliner.Term.eval_choice help_term [ run_term; list_term; analyze_term; plot_term] with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok () -> ()

