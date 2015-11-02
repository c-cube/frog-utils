
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Manage TPTP provers} *)

module StrMap = Map.Make(String)
module St = FrogMapState
module Prover = FrogTPTP.Prover
module Conf = FrogConfig
module PB = FrogPrintBox

(** Type Definitions *)
(* TODO: have a module to define times and operations on them ? *)
type time = {
  real : float;
  user : float;
  system : float;
}

type file_summary = {
  prover : string;
  mutable num_all : int;
  mutable set_unsat : time StrMap.t;  (* pb -> time *)
  mutable set_sat : time StrMap.t; (* pb -> time *)
  mutable num_error : int;
  mutable solved_time : time; (* of successfully solved problems *)
  mutable run_time : time; (* total run time of the prover *)
}

type run_params = {
  timeout : int option;
  memory : int option;
}
type analyse_params = {
  get_time : time -> float;
  filter : file_summary StrMap.t -> string -> St.result -> bool;
}

type plot_data =
  | Unsat_Time

type plot_legend =
  | Prover
  | File

type plot_drawer =
  | Simple of bool (* should we sort the list ? *)
  | Cumul of bool * int * int (* sort, filter, count *)

type plot_params = {
  analyse : analyse_params;
  graph : FrogPlot.graph_config;
  data : plot_data;
  legend : plot_legend;
  drawer : plot_drawer;
  out_file : string;
  out_format : string;
}

(* Misc function *)
let debug fmt = FrogDebug.debug fmt

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

module Opt = struct
  let (>>=) o f = match o with
    | None -> None
    | Some x -> f x
end

let add_time t t' = {
  real = t.real +. t'.real;
  user = t.user +. t'.user;
  system = t.system +. t'.system;
}

let time_of_res res = {
  real = res.St.res_rtime;
  user = res.St.res_utime;
  system = res.St.res_stime;
}

(* compute summary of this file *)
let make_summary ?(filter=(fun _ _ -> true)) prover_name prover job results =
  let s = {
    prover = prover_name;
    num_all = List.length job.St.arguments;
    set_unsat = StrMap.empty;
    set_sat = StrMap.empty;
    num_error = 0;
    solved_time = { real = 0.; user = 0.; system = 0.; };
    run_time = { real = 0.; user = 0.; system = 0.; };
  } in
  let re_sat = Opt.(prover.Prover.sat >>= compile_re ~msg:"sat") in
  let re_unsat = Opt.(prover.Prover.unsat >>= compile_re ~msg:"unsat") in
  (*
  let re_unknown = Opt.(prover.Prover.unknown >>= compile_re ~msg:"unknown") in
  *)
  StrMap.iter
    (fun file res ->
       if filter file res then begin
         let time = time_of_res res in
         s.run_time <- add_time s.run_time time;
         if res.St.res_errcode <> 0 then s.num_error <- s.num_error + 1;
         if execp_re_maybe re_sat res.St.res_out then begin
           s.solved_time <- add_time s.solved_time time;
           s.set_sat <- StrMap.add file time s.set_sat
         end else if execp_re_maybe re_unsat res.St.res_out then begin
           s.solved_time <- add_time s.solved_time time;
           s.set_unsat <- StrMap.add file time s.set_unsat
         end
      end
    ) results;
  s

let map_summaries params items =
  let aux ?filter l = List.fold_left
      (fun map (file,p_name,p,job,results) ->
         let summary = make_summary ?filter p_name p job results in
         StrMap.add file summary map
      ) StrMap.empty l
  in
  let map = aux items in
  aux ~filter:(params.filter map) items

(* obtain the full list of problems/results deal with in this file *)
let extract_file file =
  St.fold_state
    (fun (job,map) res ->
      job, StrMap.add res.St.res_arg res map
    ) (fun job -> job, StrMap.empty) file
    |> Lwt_main.run

(* Single summary analysis *)
let print_single_summary prover s =
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

let analyse_single_file ~config prover file =
  let p = Prover.find_config config prover in
  let job, results = extract_file file in
  let s = make_summary prover p job results in
  print_single_summary prover s

let all_proved s =
  StrMap.merge (fun _ _ _ -> Some ()) s.set_unsat s.set_sat

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
        |> StrMap.merge (fun _ _ _ -> Some ()) s.set_unsat
        |> StrMap.merge (fun _ _ _ -> Some ()) s.set_sat
      ) others acc
  with Not_found ->
    StrMap.empty

let map_diff m1 m2 =
  StrMap.merge
    (fun _ x1 x2 -> match x1, x2 with
      | Some x, None -> Some x
      | _ -> None
    ) m1 m2

(* analyse and compare this list of prover,job,results *)
let analyse_multiple params items =
  let map = map_summaries params items in
  (* some globals *)
  let problems_solved_by_one = ref StrMap.empty in
  (* print overall *)
  let first_line = PB.(
      [| text "file"; text "prover"; text "sat"; text "unsat"; text "total"; text "exclusive";
         text "%solved"; text "time (s)"; text "avg time (s)"; text "errors" ; text "runtime (s)" |]
  ) in
  (* next lines *)
  let next_lines = List.rev (* keep ascending order of file names *)
    @@ StrMap.fold
      (fun file s acc ->
         let prover = s.prover in
         let problems_solved_by_me = all_proved s in
         let problems_solved_by_others = all_proved_map (StrMap.remove file map) in
         let problems_solved_by_me_only = map_diff
             problems_solved_by_me problems_solved_by_others
         in
         problems_solved_by_one := StrMap.add prover
             problems_solved_by_me_only !problems_solved_by_one;
         let num_sat = StrMap.cardinal s.set_sat in
         let num_unsat = StrMap.cardinal s.set_unsat in
         let percent_solved = (float (num_sat + num_unsat) *. 100. /. float s.num_all) in
         let num_solved_only = StrMap.cardinal problems_solved_by_me_only in
         PB.([| text @@ Filename.basename file; text prover; int_ num_sat; int_ num_unsat; int_ s.num_all;
                int_ num_solved_only;
                text (Printf.sprintf "%.0f" percent_solved);
                text (Printf.sprintf "%.2f" (params.get_time s.solved_time));
                text (Printf.sprintf "%.2f"
                        ((params.get_time s.solved_time) /.
                         float (StrMap.cardinal s.set_sat + StrMap.cardinal s.set_unsat)));
                int_ s.num_error;
                text (Printf.sprintf "%.2f" (params.get_time s.run_time)) |]
            ) :: acc
      ) map []
  in
  let box = PB.(frame (grid (Array.of_list (first_line :: next_lines)))) in
  print_endline "";
  PB.output stdout box;
  print_endline "";
  (* TODO: pairwise comparison *)
  (* print, for each prover, list of problems it's the only one to solve *)
  StrMap.iter
    (fun prover map ->
      Printf.printf "problems solved by only %s:\n" prover;
      StrMap.iter
        (fun file () -> Printf.printf "  %s\n" file)
        map
    ) !problems_solved_by_one;
  ()

let parse_prover_list ~config l =
  List.map
    (fun (prover,file) ->
       let p = Prover.find_config config prover in
       let job, results = extract_file file in
       file, prover, p, job, results
    ) l

let analyse ~config params l = match l with
  | [] -> assert false
  | [p, file] ->
      debug "analyse file %s, obtained from prover %s" file p;
      analyse_single_file ~config p file
  | _ ->
      debug "analyse %d files" (List.length l);
      analyse_multiple params (parse_prover_list ~config l)

(* Plot functions *)
let plot ~config params l =
  let items = parse_prover_list ~config l in
  let map = map_summaries params.analyse items in
  let datas = StrMap.fold (fun file s acc ->
      let name = match params.legend with
        | Prover -> s.prover | File -> Filename.basename file
      in
      let l = match params.data with
        | Unsat_Time -> List.rev @@ StrMap.fold (fun _ time acc ->
            params.analyse.get_time time :: acc) s.set_unsat []
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
    (fun name p ->
      Printf.printf "  %s: cmd=%s\n" name p.Prover.cmd;
    ) provers

(** {2 Run} *)

(* Argument parsing *)
let use_time =
  (function
    | "real" -> `Ok (fun { real; _ } -> real)
    | "user" -> `Ok (fun { user; _ } -> user)
    | "sys"  -> `Ok (fun { system; _ } -> system)
    | "cpu"  -> `Ok (fun { user; system; _ } -> user +. system)
    | _ -> `Error "Must be one of : 'real', 'user', 'sys', or 'cpu'"),
  (fun fmt _ -> Format.fprintf fmt "*abstract*")

let filter_pbs =
  (function
    | "all" -> `Ok (fun _ _ _ -> true)
    | "common" -> `Ok (fun map problem _ ->
        StrMap.for_all (fun _ s -> StrMap.mem problem s.set_unsat) map)
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
  let doc = "List of pairs of prover and corresponding output file to analyse" in
  Arg.(non_empty & pos 0 (list (pair ~sep:'=' string non_dir_file)) [] & info [] ~doc)

let config_term =
  let open Cmdliner in
  let aux config debug =
    if debug then begin
      FrogDebug.set_debug true;
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
  let aux memory timeout = {memory = some_if_pos_ memory; timeout = some_if_pos_ timeout;} in
  let memory =
    let doc = "Memory limit" in
    Arg.(value & opt int (~- 1) & info ["m"; "memory"] ~doc)
  in
  let timeout =
    let doc = "Time limit" in
    Arg.(value & opt int (~- 1) & info ["t"; "timeout"] ~doc)
  in
  Term.(pure aux $ memory $ timeout)

let analyze_params_term =
  let open Cmdliner in
  let aux get_time filter = { get_time; filter; } in
  let use_time =
    let doc = "Specify which time to use for comparing provers. Choices are :
               'real' (real time elasped between start and end of execution),
               'user' (user time, as defined by the 'time 'command),
               'sys' (system time as defined by the 'time' command),
               'cpu' (sum of user and system time)." in
    Arg.(value & opt use_time (fun { real; _} -> real) & info ["t"; "time"] ~doc)
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
    FrogTPTP.run_exec
      ?timeout:params.timeout
      ?memory:params.memory
      ~config cmd (String.concat " " args)
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

