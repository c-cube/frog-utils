
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Cmdliner

let main db_path port =
  let s = FrogWeb.Server.create
      ~db_init:[
        FrogProver.db_init;
        FrogProblem.db_init;
        FrogRun.db_init;
      ] ~db_path ~port
      ()
  in
  FrogProver.add_server s;
  FrogProblem.add_server s;
  FrogRun.add_server s;
  let web = FrogWeb.Server.run s in
  Lwt_main.run web

let term =
  let db =
    let doc = "Path to the sqlite db" in
    Arg.(value & opt non_dir_file "$HOME/.frogdb.sql" & info ["db"] ~doc)
  in
  let port =
    let doc = "Port on which to run the web server" in
    Arg.(value & opt int 8000 & info ["p"; "port"] ~doc)
  in
  Term.(pure main $ db $ port)

let parse_opt () =
  let help =
    let doc = "Start a webservice to diplay the results in the db" in
    Term.info ~version:"dev" ~doc "frogweb"
  in
  Term.eval (term, help)

let () =
  match parse_opt () with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok () -> ()

