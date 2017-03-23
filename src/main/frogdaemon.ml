
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Main daemon for IPC} *)

let section = Lwt_log.Section.make "frogdaemon"

let main_config_file = "/etc/froglock.conf"

let setup_loggers file_name : unit Lwt.t =
  let syslog = Lwt_log.syslog ~facility:`User () in
  Lwt_log.default := syslog;
  let%lwt () =
    try%lwt
      let%lwt log' = Lwt_log.file ~mode:`Append ~perm:0o666 ~file_name () in
      let all_log = Lwt_log.broadcast [log'; syslog] in
      Lwt_log.default := all_log;
      Lwt.return_unit
    with e ->
      let%lwt _ = Lwt_io.eprintlf "error opening log file %s" file_name in
      Lwt_log.ign_error_f "could not open file %s: %s"
        file_name (Printexc.to_string e);
      Lwt.return_unit
  in
  Lwt_io.close Lwt_io.stderr


(* daemonize and listen *)
let main (port:int): unit =
  Lwt_daemon.daemonize ~syslog:true ~directory:"/tmp"
    ~stdin:`Close ~stdout:`Close ~stderr:`Keep ();
  let log_file =
    let config =
      if Sys.file_exists main_config_file
      then Config.parse_or_empty main_config_file
      else Config.empty
    in
    Config.get_string ~default:"/tmp/froglock.log" config "log"
  in
  Lwt_log.add_rule "*" Lwt_log.Debug;
  Lwt_log.ign_debug ~section "loggers are setup";
  let thread =
    let%lwt () = setup_loggers log_file in
    try%lwt
      IPC_daemon.spawn ~forever:true port
    with e ->
      Lwt_log.ign_error_f ~section "daemon: error: %s" (Printexc.to_string e);
      Lwt.return_unit
  in
  Lwt_main.run thread

let term =
  let open Cmdliner in
  let port =
    Arg.(value & opt int IPC_daemon.default_port & info ["p"; "port"] ~doc:"port to listen on")
  in
  let doc = "Run main IPC daemon" in
  let man = [
    `S "SYNOPSIS";
    `I ("$(b,frogdaemon [OPTIONS])", "Run daemon");
  ] in
  Term.(pure main $ port),
  Term.info ~man ~doc "frogdaemon"

let () =
  match Cmdliner.Term.eval term with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok () -> ()
