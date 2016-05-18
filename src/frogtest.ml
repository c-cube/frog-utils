
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(* run tests, or compare results *)

open Result
module T = FrogTest
module Prover = FrogProver
module E = FrogMisc.LwtErr
module W = FrogWeb

type save =
  | SaveFile of string
  | SaveCommit  (* save into a file named by the current git commit *)
  | SaveNone  (* no save *)

(** {2 Run} *)
module Run = struct
  (* callback that prints a result *)
  let on_solve pb res =
    let module F = FrogMisc.Fmt in
    let pp_res out () =
      let str, c = match T.Problem.compare_res pb res with
        | `Same -> "ok", `Green
        | `Improvement -> "ok (improved)", `Blue
        | `Disappoint -> "disappoint", `Yellow
        | `Mismatch -> "bad", `Red
      in
      Format.fprintf out "%a" (F.in_bold_color c Format.pp_print_string) str
    in
    Format.printf "problem %-50s %a@." (pb.T.Problem.name ^ " :") pp_res ();
    Lwt.return_unit

  (* save result in given file *)
  let save_ ~file res =
    Lwt_log.ign_debug_f "save result in file `%s`" file;
    T.Results.to_file ~file res;
    Lwt.return_unit

  (* obtain the current commit name *)
  let get_commit_ () : string Lwt.t =
    let open Lwt.Infix in
    Lwt_process.with_process_in
      (Lwt_process.shell "git rev-parse HEAD")
      (fun p -> FrogMisc.File.read_all p#stdout >|= String.trim)

  (* lwt main *)
  let main ?j ?timeout ?memory ?caching ~web ~db ~save ~config ~dir () =
    let open E in
    (* parse config *)
    Lwt.return (T.Config.of_file (Filename.concat dir config))
    >>= fun config ->
    (* build problem set (exclude config file!) *)
    let problem_pat = Re_posix.compile_pat config.T.Config.problem_pat in
    T.ProblemSet.of_dir ~filter:(Re.execp problem_pat) dir
    >>= fun pb ->
    Format.printf "run %d tests in %s@." (T.ProblemSet.size pb) dir;
    (* serve website *)
    let server = if web then Some (W.Server.create ~db_path:db ()) else None in
    (* solve *)
    let main =
      E.ok (T.run ?j ?timeout ?memory ?caching ~on_solve ?server ~config pb)
    in
    let web = FrogMisc.Opt.((server >|= W.Server.run) |> get Lwt.return_unit) |> E.ok in
    main >>= fun results ->
    Format.printf "%a@." T.Results.print results;
    let%lwt () = match save with
      | SaveFile f -> save_ ~file:f results
      | SaveCommit ->
          let%lwt commit = get_commit_ () in
          Lwt_log.ign_debug_f "commit name: %s" commit;
          save_ ~file:(commit ^ ".json") results
      | SaveNone ->
          Lwt_log.ign_debug "do not save result";
          Lwt.return_unit
    in
    let%lwt _ = web in
    if T.Results.is_ok results
    then E.return () (* wait for webserver to return *)
    else
      E.fail (Format.asprintf "%d failure(s)" (T.Results.num_failed results))
end

(** {2 Display Results} *)
module Display = struct
  let main ~file () =
    let open E in
    E.lift (T.Results.of_file ~file) >>= fun res ->
    Format.printf "%a@." T.Results.print res;
    E.return ()
end

(** {2 Compare Results} *)
module Compare = struct
  let main ~file1 ~file2 () =
    let open E in
    E.lift (T.Results.of_file ~file:file1) >>= fun res1 ->
    E.lift (T.Results.of_file ~file:file2) >>= fun res2 ->
    let cmp = T.ResultsComparison.compare res1.T.Results.raw res2.T.Results.raw in
    Format.printf "%a@." T.ResultsComparison.print cmp;
    E.return ()
end

(** {2 Main: Parse CLI} *)

(* sub-command for running tests *)
let term_run =
  let open Cmdliner in
  let aux debug config save dir j timeout memory nocaching web db =
    if debug then (
      Maki_log.set_level 5;
      Lwt_log.add_rule "*" Lwt_log.Debug;
    );
    Lwt_main.run (Run.main ?j ?timeout ?memory ~caching:(not nocaching) ~web ~db ~save ~config ~dir ())
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
  and memory = Arg.(value & opt (some int) None &
    info ["m"; "memory"] ~doc:"memory (in MB)")
  and web = Arg.(value & flag & info ["web"] ~doc:"embedded web server")
  and db = Arg.(value & opt string "frogtest" & info ["db"] ~doc:"path to database")
  and nocaching = Arg.(value & flag & info ["no-caching"] ~doc:"toggle caching")
  and doc = "test a program on every file in a directory" in
  Term.(pure aux $ debug $ config $ save $ dir $ j $ timeout $ memory $ nocaching $ web $ db), Term.info ~doc "run"

(* sub-command to display a file *)
let term_display =
  let open Cmdliner in
  let aux file = Lwt_main.run (Display.main ~file ()) in
  let file =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc:"file containing results")
  and doc = "display test results from a file" in
  Term.(pure aux $ file), Term.info ~doc "display"

(* sub-command to compare two files *)
let term_compare =
  let open Cmdliner in
  let aux file1 file2 = Lwt_main.run (Compare.main ~file1 ~file2 ()) in
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
  | `Ok (Ok ()) -> ()
  | `Ok (Error e) ->
      print_endline ("error: " ^ e);
      exit 1
