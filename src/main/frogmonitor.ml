
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Monitor Events} *)

open Frog
open Frog_server

module E = Misc.LwtErr

let main port debug =
  let open E.Infix in
  if debug then Lwt_log.add_rule "*" Lwt_log.Debug;
  IPC_client.connect_or_spawn port
    (fun c ->
       Lwt_log.ign_debug_f "opened connection to daemon";
       (* print every message *)
       IPC_client.on_msg c
         (fun msg ->
            Printf.printf "read: %s\n%!" (IPC_message.show msg);
            `Continue);
       IPC_client.wait_close c)
  |> Lwt_main.run

let term =
  let open Cmdliner in
  let port =
    Arg.(value & opt int IPC_daemon.default_port & info ["p"; "port"] ~doc:"port to listen on")
  and debug  =
    Arg.(value & flag & info ["d"; "debug"] ~doc:"activate debug")
  in
  let doc = "monitor events" in
  let man = [
    `S "SYNOPSIS";
    `I ("$(b,frogmonitor [OPTIONS])", "monitor daemon");
  ] in
  Term.(pure main $ port $ debug),
  Term.info ~man ~doc "frogmonitor"

let () =
  match Cmdliner.Term.eval term with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok () -> ()
