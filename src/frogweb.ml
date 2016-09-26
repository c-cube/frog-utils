
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Frog
open Cmdliner

let main db_path port =
  let db =
    DB.create ~db_init:[
        Prover.db_init;
        Problem.db_init;
        Run.db_init;
    ] ~db_path ()
  in
  let s = Web.Server.create ~port ~db () in
  Prover.add_server s;
  Problem.add_server s;
  Run.add_server s;
  let web = Web.Server.run s in
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

