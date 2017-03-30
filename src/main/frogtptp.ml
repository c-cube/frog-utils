
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Manage TPTP provers} *)

open Frog
open Frog_server

module StrMap = Map.Make(String)
module Conf = Config
module Opt = CCOpt

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

(* print list of known provers *)
let list_provers ~config =
  let provers = match ProverSet.of_config config with
    | Ok p -> p
    | Error e -> failwith e
  in
  Printf.printf "provers:\n";
  StrMap.iter
    (fun name p -> Printf.printf "  %s: cmd=%s\n" name p.Prover.cmd)
    provers

(** {2 Run} *)

(* Argument parsing *)
let use_time =
  (function
    | "real" -> `Ok (fun { TPTP.real; _ } -> real)
    | "user" -> `Ok (fun { TPTP.user; _ } -> user)
    | "sys"  -> `Ok (fun { TPTP.system; _ } -> system)
    | "cpu"  -> `Ok (fun { TPTP.user; system; _ } -> user +. system)
    | _ -> `Error "Must be one of : 'real', 'user', 'sys', or 'cpu'"),
  (fun fmt _ -> Format.fprintf fmt "*abstract*")

let filter_pbs =
  (function
    | "all" -> `Ok (fun _ _ _ -> true)
    | "common" ->
      `Ok
        (fun map problem _ ->
           StrMap.for_all
             (fun _ s -> StrMap.mem problem s.TPTP.set_unsat)
             map)
    | _ -> `Error "Must be one of : 'all', 'common'"),
  (fun fmt _ -> Format.fprintf fmt "*abstract*")

let to_cmd_arg l = Cmdliner.Arg.enum l, Cmdliner.Arg.doc_alts_enum l

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
    let config = Config.interpolate_home config in
    if debug then (
      Maki_log.set_level 5;
      Lwt_log.add_rule "*" Lwt_log.Debug;
    );
    begin match Config.parse_file config with
      | Ok p -> `Ok p
      | Error msg -> `Error (false, msg)
    end
  in
  let arg =
    Arg.(value & opt string "$home/.frogtptp.toml" &
         info ["c"; "config"] ~doc:"configuration file (in target directory)")
  and debug =
    let doc = "Enable debug (verbose) output" in
    Arg.(value & flag & info ["d"; "debug"] ~doc)
  in
  Term.(ret (pure aux $ arg $ debug))

let limit_term =
  let open Cmdliner in
  let aux memory timeout =
    { TPTP.memory = memory; timeout = timeout;}
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

let run_term =
  let open Cmdliner in
  let aux config params cmd file =
    let test_tbl = Config.table "test" in
    let timeout =
      match params.TPTP.timeout with
      | Some res -> res
      | None ->
        begin match Config.(get config @@ (try_tables [test_tbl; top] @@ int "timeout")) with
          | Ok x -> x
          | Error _ ->
            failwith "A timeout is required (either on the command line or in the config file)"
        end
    in
    let memory =
      match params.TPTP.memory with
      | Some res -> res
      | None ->
        begin match Config.(get config @@ (try_tables [test_tbl; top] @@ int "memory")) with
          | Ok x -> x
          | Error _ ->
            failwith "A memory limit is required (either on the command line or in the config file)"
        end
    in
    let prover = match ProverSet.find_config config cmd with
      | Error e -> failwith e
      | Ok p -> p
    in
    Run.TPTP.exec_prover ~config ~timeout ~memory ~prover ~file ()
  in
  let cmd =
    let doc = "Prover to be run" in
    Arg.(required & pos 0 (some string) None & info [] ~doc)
  in
  let file =
    let doc = "Arguments to be passed to the prover" in
    Arg.(required & pos 1 (some string) None & info [] ~doc)
  in
  let doc = "Run the prover on the given arguments" in
  let man = [
    `S "DESCRIPTION";
    `P "This tool allows to run provers with pre-set options and uniform options
        for time and memory limits. Provers to be run must be present in one of the
        configuration files specified with the options.";
  ] in
  Term.(pure aux $ config_term $ limit_term $ cmd $ file),
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
  match Cmdliner.Term.eval_choice help_term [ run_term; list_term; ] with
    | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
    | `Ok () -> ()

