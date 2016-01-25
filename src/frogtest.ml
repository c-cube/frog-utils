
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(* run tests, or compare results *)

module T = FrogTest
module Prover = FrogProver

type save =
  | SaveFile of string
  | SaveCommit  (* save into a file named by the current git commit *)
  | SaveNone  (* no save *)

(** {2 Run} *)
module Run = struct
  (* callback that prints a result *)
  let on_solve pb res =
    let pp_res out () =
      Format.fprintf out
        (match T.Problem.compare_res pb res with
        | `Same -> "@{<Green>ok@}"
        | `Improvement -> "@{<Blue>ok (improved)@}"
        | `Mismatch -> "@{<Red>bad@}"
        )
    in
    Format.printf "problem %-50s %a@." (pb.T.Problem.name ^ " :") pp_res ();
    ()

  (* save result in given file *)
  let save_ ~file res =
    Logs.debug (fun k->k "save result in file %s" file);
    T.Results.to_file ~file res;
    ()

  (* obtain the current commit name *)
  let get_commit_ () : string =
    CCUnix.with_process_in "git rev-parse HEAD"
      ~f:(fun oc -> CCIO.read_all oc |> String.trim)

  (* lwt main *)
  let main ?j ?timeout ~save ~config ~dir () =
    let open CCError.Infix in
    (* parse config *)
    T.Config.of_file (Filename.concat dir config)
    >>= fun config ->
    (* build problem set (exclude config file!) *)
    T.ProblemSet.of_dir ~filter:(Re.execp config.T.Config.problem_pat) dir
    >>= fun pb ->
    Format.printf "run %d tests in %s@." (T.ProblemSet.size pb) dir;
    (* solve *)
    CCError.return (T.run ?j ?timeout ~on_solve ~config pb)
    >>= fun results ->
    Format.printf "%a@." T.Results.print results;
    begin match save with
      | SaveFile f -> save_ ~file:f results
      | SaveCommit ->
          let commit = get_commit_ () in
          Logs.debug (fun k->k "commit name: %s" commit);
          save_ ~file:(commit ^ ".json") results
      | SaveNone ->
          Logs.debug (fun k->k "do not save result");
          ()
    end;
    if T.Results.is_ok results
    then CCError.return ()
    else
      CCError.fail (Format.asprintf "%d failure(s)" (T.Results.num_failed results))
end

(** {2 Display Results} *)
module Display = struct
  let main ~file () =
    let open CCError.Infix in
    T.Results.of_file ~file >|= fun res ->
    Format.printf "%a@." T.Results.print res;
    ()
end

(** {2 Compare Results} *)
module Compare = struct
  let main ~file1 ~file2 () =
    let open CCError.Infix in
    T.Results.of_file ~file:file1 >>= fun res1 ->
    T.Results.of_file ~file:file2 >|= fun res2 ->
    let cmp = T.ResultsComparison.compare res1.T.Results.raw res2.T.Results.raw in
    Format.printf "%a@." T.ResultsComparison.print cmp;
    ()
end

(** {2 Main: Parse CLI} *)

(* sub-command for running tests *)
let term_run =
  let open Cmdliner in
  let aux debug config save dir j timeout =
    (* FIXME
    FrogDebug.set_debug debug;
    *)
    Run.main ?j ?timeout ~save ~config ~dir ()
  in
  let save =
    let parse_ = function
      | "commit" -> `Ok SaveCommit
      | "none" -> `Ok SaveNone
      | s ->
          try Scanf.sscanf s "file=%s" (fun f -> `Ok (SaveFile f))
          with _ -> `Error "expected (commit|none|file=<filename>)"
    and print_ out = function
      | SaveCommit -> Format.fprintf out "commit"
      | SaveNone -> Format.fprintf out "none"
      | SaveFile f -> Format.fprintf out "file=%s" f
    in Arg.(value & opt (parse_,print_) SaveNone &
      info ["s"; "save"] ~doc:"indicate where to save results")
  in
  let debug = Arg.(value & flag & info ["d"; "debug"] ~doc:"enable debug")
  and config = Arg.(value & opt string "test.toml" &
    info ["c"; "config"] ~doc:"configuration file (in target directory)")
  and dir = Arg.(value & pos 0 string "./" &
    info [] ~docv:"DIR" ~doc:"target directory (containing tests)")
  and j = Arg.(value & opt (some int) None & info ["j"] ~doc:"parallelism level")
  and timeout = Arg.(value & opt (some int) None &
    info ["t"; "timeout"] ~doc:"timeout (in s)")
  and doc = "test a program on every file in a directory" in
  Term.(pure aux $ debug $ config $ save $ dir $ j $ timeout), Term.info ~doc "run"

(* sub-command to display a file *)
let term_display =
  let open Cmdliner in
  let aux file = Display.main ~file () in
  let file =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc:"file containing results")
  and doc = "display test results from a file" in
  Term.(pure aux $ file), Term.info ~doc "display"

(* sub-command to compare two files *)
let term_compare =
  let open Cmdliner in
  let aux file1 file2 = Compare.main ~file1 ~file2 () in
  let file1 = Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE1" ~doc:"first file")
  and file2 = Arg.(required & pos 1 (some string) None & info [] ~docv:"FILE2" ~doc:"second file")
  and doc = "compare two result files" in
  Term.(pure aux $ file1 $ file2), Term.info ~doc "compare"

let parse_opt () =
  let open Cmdliner in
  let help =
    let doc = "Offers various utilities to test automated theorem provers." in
    let man = [
      `S "DESCRIPTION";
      `P "$(b,frogtest) is a set of utils to run tests, save their results,
          and compare different results obtained with distinct versions of
          the same tool";
      `S "COMMANDS";
      `S "OPTIONS"; (* TODO: explain config file *)
    ] in
    Term.(ret (pure (fun () -> `Help (`Pager, None)) $ pure ())),
    Term.info ~version:"dev" ~man ~doc "frogtest"
  in
  Cmdliner.Term.eval_choice
    help [ term_run; term_compare; term_display ]

let () =
  match parse_opt () with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok (`Ok ()) -> ()
  | `Ok (`Error e) ->
      print_endline ("error: " ^ e);
      exit 1
