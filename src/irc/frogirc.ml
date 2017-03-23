
open Frog_server
module C = Calculon

(* TODO: cmdliner for parsing this *)
let config = {
  C.Config.default with C.Config.channel = "#deducteam"; nick = "froggy"; }

let main_lwt port =
  let plugins =
    [ Frog_irc.plugin ~port ();
      C.Plugin_factoids.plugin;
      C.Plugin_history.plugin ();
    ] in
  C.Run_main.main config plugins

let main port debug =
  if debug then (
    Lwt_log.add_rule "*" Lwt_log.Debug;
  );
  Lwt_main.run (main_lwt port);
  ()

let term =
  let open Cmdliner in
  let port =
    Arg.(value & opt int IPC_daemon.default_port & info ["p"; "port"] ~doc:"port for the daemon")
  and debug  =
    Arg.(value & flag & info ["d"; "debug"] ~doc:"activate debug")
  in
  let doc = "irc client" in
  let man = [
    `S "SYNOPSIS";
    `I ("$(b,frogirc [OPTIONS])", "irc client");
  ] in
  Term.(pure main $ port $ debug),
  Term.info ~man ~doc "frogclient"

let () =
  match Cmdliner.Term.eval term with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok () -> ()
