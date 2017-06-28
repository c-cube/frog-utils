
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Frog
open Frog_server
open Cmdliner

let main storage_dirs port debug =
  if debug then Lwt_log.add_rule "*" Lwt_log.Debug;
  let storage = Storage.make storage_dirs in
  let s = Web.Server.create ~port ~storage () in
  let web = Web.Server.run s in
  Lwt_main.run web

let term =
  let db_path =
    let doc = "on-disk storage database" in
    Arg.(value & opt string "~/.frogutils/frog.db" & info ["db"] ~doc)
  and port =
    let doc = "Port on which to run the web server" in
    Arg.(value & opt int 8000 & info ["p"; "port"] ~doc)
  and debug =
    Arg.(value & flag & info ["debug"] ~doc:"enable debug messages")
  in
  Term.(pure main $ db_path $ port $ debug)

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

