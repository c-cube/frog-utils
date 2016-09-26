
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Frog
open Cmdliner

let main storage_dirs port =
  let storage = Storage.make storage_dirs in
  let s = Web.Server.create ~port ~storage () in
  Problem.add_server s;
  let web = Web.Server.run s in
  Lwt_main.run web

let term =
  let storage_dirs =
    let doc = "on-disk storage directories" in
    Arg.(value & opt (list string) ["$HOME/.frogutils"] & info ["storage"] ~doc)
  in
  let port =
    let doc = "Port on which to run the web server" in
    Arg.(value & opt int 8000 & info ["p"; "port"] ~doc)
  in
  Term.(pure main $ storage_dirs $ port)

let parse_opt () =
  let help =
    let doc = "Start a webservice to diplay the results" in
    Term.info ~version:"dev" ~doc "frogweb"
  in
  Term.eval (term, help)

let () =
  match parse_opt () with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok () -> ()

