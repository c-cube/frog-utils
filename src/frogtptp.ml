
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

(** {2 Description of a prover} *)

let debug fmt = FrogDebug.debug fmt

let re_not_comma = Re_posix.compile_pat "[^,]+"

(* split a string along "," *)
let split_comma s =
  let l = ref [] in
  let i = ref 0 in
  try
    while !i < String.length s do
      let sub = Re.exec ~pos:!i re_not_comma s in
      l := Re.get sub 0 :: !l;
      let start, len = Re.get_ofs sub 0 in
      i := start + len;
    done;
    List.rev !l
  with Not_found ->
    List.rev !l

type file_summary = {
  mutable num_all : int;
  mutable set_unsat : float StrMap.t;  (* pb -> time *)
  mutable set_sat : float StrMap.t; (* pb -> time *)
  mutable num_error : int;
  mutable total_time : float; (* of successfully solved problems *)
}

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

(* compute summary of this file *)
let make_summary prover job results =
  let s = {
    num_all=List.length job.St.arguments;
    set_unsat=StrMap.empty;
    set_sat=StrMap.empty;
    num_error=0;
    total_time=0.;
  } in
  let re_sat = Opt.(prover.Prover.sat >>= compile_re ~msg:"sat") in
  let re_unsat = Opt.(prover.Prover.unsat >>= compile_re ~msg:"unsat") in
  (*
  let re_unknown = Opt.(prover.Prover.unknown >>= compile_re ~msg:"unknown") in
  *)
  StrMap.iter
    (fun file res ->
      if res.St.res_errcode <> 0
        then s.num_error <- s.num_error + 1;
      if execp_re_maybe re_sat res.St.res_out then (
        s.total_time <- s.total_time +. res.St.res_time;
        s.set_sat <- StrMap.add file res.St.res_time s.set_sat
      ) else if execp_re_maybe re_unsat res.St.res_out then (
        s.total_time <- s.total_time +. res.St.res_time;
        s.set_unsat <- StrMap.add file res.St.res_time s.set_unsat
      );
    ) results;
  s

module PB = PrintBox

let print_single_summary prover s =
  let num_sat = StrMap.cardinal s.set_sat in
  let num_unsat = StrMap.cardinal s.set_unsat in
  let percent_solved = (float (num_sat + num_unsat) *. 100. /. float s.num_all) in
  Printf.printf "prover %s: %d sat, %d unsat, %d total (%.0f%% solved) in %.2fs, %d errors\n"
    prover num_sat num_unsat s.num_all percent_solved s.total_time s.num_error;
  ()

(* obtain the full list of problems/results deal with in this file *)
let extract_file file =
  St.fold_state
    (fun (job,map) res ->
      job, StrMap.add res.St.res_arg res map
    ) (fun job -> job, StrMap.empty) file
    |> Lwt_main.run

let analyse_single_file ~config prover file =
  let p = Prover.find_config config prover in
  let job, results = extract_file file in
  let s = make_summary p job results in
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
let analyse_multiple items =
  (* individual results *)
  let map = List.fold_left
    (fun map (p_name,p,job,results) ->
      let summary = make_summary p job results in
      (* individual summary *)
      StrMap.add p_name summary map
    ) StrMap.empty items
  in
  (* some globals *)
  let problems_solved_by_one = ref StrMap.empty in
  (* print overall *)
  let first_line = PB.(
    [| text "prover"; text "sat"; text "unsat"; text "total"; text "exclusive";
       text "%solved"; text "time (s)"; text "avg time (s)"; text "errors" |]
  ) in
  (* next lines *)
  let next_lines = StrMap.fold
    (fun prover s acc ->
      let problems_solved_by_me = all_proved s in
      let problems_solved_by_others = all_proved_map (StrMap.remove prover map) in
      let problems_solved_by_me_only = map_diff
        problems_solved_by_me problems_solved_by_others
      in
      problems_solved_by_one := StrMap.add prover
        problems_solved_by_me_only !problems_solved_by_one;
      let num_sat = StrMap.cardinal s.set_sat in
      let num_unsat = StrMap.cardinal s.set_unsat in
      let percent_solved = (float (num_sat + num_unsat) *. 100. /. float s.num_all) in
      let num_solved_only = StrMap.cardinal problems_solved_by_me_only in
      PB.([| text prover; int_ num_sat; int_ num_unsat; int_ s.num_all;
             int_ num_solved_only;
             text (Printf.sprintf "%.0f" percent_solved);
             text (Printf.sprintf "%.2f" s.total_time);
             text (Printf.sprintf "%.2f" (s.total_time /. float s.num_all));
             int_ s.num_error |]
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

let analyse_multiple_files ~config l =
  let items = List.map
    (fun (prover,file) ->
      let p = Prover.find_config config prover in
      let job, results = extract_file file in
      prover, p, job, results
    ) l
  in
  analyse_multiple items

let analyse ~config l = match l with
  | [] -> assert false
  | [p, file] ->
      debug "analyse file %s, obtained from prover %s" file p;
      analyse_single_file ~config p file
  | _ ->
      debug "analyse %d files" (List.length l);
      analyse_multiple_files ~config l

(* print list of known provers *)
let list_provers ~config =
  let provers = Prover.of_config config in
  Printf.printf "provers:\n";
  StrMap.iter
    (fun name p ->
      Printf.printf "  %s: cmd=%s\n" name p.Prover.cmd;
    ) provers

(** {2 Run} *)

type cmd =
  | Analyse of (string * string) list  (* list (prover, file) *)
  | Run of string * string
  | ListProvers

type specific_conf = {
  timeout : int option;
  memory : int option;
}

type params = {
  cmd : cmd;
  config_files : string list;
  conf : specific_conf;
}

let main params =
  try
    let config = FrogConfig.parse_files params.config_files FrogConfig.empty in
    match params.cmd with
    | Run (prog,args) ->
        FrogTPTP.run_exec
          ?timeout:params.conf.timeout
          ?memory:params.conf.memory
          ~config prog args
    | Analyse l ->
        analyse ~config l
    | ListProvers ->
        list_provers ~config
  with FrogConfig.Error msg ->
    print_endline msg;
    exit 1

(** {2 Main} *)

let some_if_pos_ i =
  if i>0 then Some i else None

let config_term =
  let open Cmdliner in
  let aux config debug =
    if debug then FrogDebug.set_debug true;
    config
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
  Term.(pure aux $ config $ debug)

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

let analyze_term =
  let open Cmdliner in
  let aux config_files conf l = main { config_files; conf; cmd = Analyse l } in
  let args =
    let doc = "List of pairs of prover and corresponding output file to analyse" in
    Arg.(non_empty & pos 0 (list (pair ~sep:'=' string non_dir_file)) [] & info [] ~doc)
  in
  let doc = "Analyze the results of provers run previously" in
  let man = [
    `S "DESCRIPTION";
    `P "Analyse the prover's results and prints statistics about them.
        TODO: more detailed explication.";
  ] in
  Term.(pure aux $ config_term $ limit_term $ args),
  Term.info ~man ~doc "analyse"

let run_term =
  let open Cmdliner in
  let aux config_files conf cmd args = main {config_files; conf; cmd = Run (cmd, String.concat " " args) } in
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
  let aux config_files conf = main { config_files; conf; cmd = ListProvers } in
  let doc = "List the known provers,given the config files specified (see 'config' option)" in
  let man = [
    `S "DESCRIPTION";
    `P "Prints on stdout the list of known provers according to the configuration
        files specified with the options.";
    `P "The list conatins one line for each prover, and is of the form : '<name>: cmd=<cmd>'.";
  ] in
  Term.(pure aux $ config_term $ limit_term),
  Term.info ~man ~doc "list"

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
  match Cmdliner.Term.eval_choice help_term [run_term;list_term;analyze_term] with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok () -> ()

