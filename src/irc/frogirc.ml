
open Frog_server
module C = Calculon

let main_lwt ~port ~server ~irc_port ~nick ~chan () =
  let plugins =
    [ Frog_irc.plugin ~port ();
      C.Plugin_factoids.plugin;
      C.Plugin_history.plugin ();
    ]
  and config =
    { C.Config.default with
        C.Config.channel = chan; nick; server; port = irc_port;
    }
  in
  C.Run_main.main config plugins

let main port chan irc_port nick server debug =
  if debug then (
    Lwt_log.add_rule "*" Lwt_log.Debug;
  );
  Lwt_main.run (main_lwt ~port ~irc_port ~chan ~nick ~server ());
  ()

let term =
  let open Cmdliner in
  let port =
    Arg.(value & opt int IPC_daemon.default_port & info ["p"; "port"] ~doc:"port for the daemon")
  and irc_port =
    Arg.(value & opt int 7000 & info ["irc-port"] ~doc:"port for IRC")
  and server =
    Arg.(value & opt string "irc.freenode.net" & info ["server"] ~doc:"server")
  and chan =
    Arg.(value & opt string "#deducteam" & info ["chan"] ~doc:"chan")
  and nick =
    Arg.(value & opt string "froggy" & info ["nick"] ~doc:"irc nick")
  and debug  =
    Arg.(value & flag & info ["d"; "debug"] ~doc:"activate debug")
  in
  let doc = "irc client" in
  let man = [
    `S "SYNOPSIS";
    `I ("$(b,frogirc [OPTIONS])", "irc client");
  ] in
  Term.(pure main $ port $ chan $ irc_port $ nick $ server $ debug),
  Term.info ~man ~doc "frogclient"

let () =
  match Cmdliner.Term.eval term with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok () -> ()
