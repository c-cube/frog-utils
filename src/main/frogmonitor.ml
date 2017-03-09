
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Monitor Events} *)

open Frog
open Frog_server

module E = Misc.LwtErr

let main () =
  let open E.Infix in
  IPC_client.connect_or_spawn IPC_daemon.default_port
    (fun c ->
       Lwt_log.ign_debug_f "opened connection to daemon";
       (* print every message *)
       IPC_client.on_msg c
         (fun msg ->
            Printf.printf "read: %s\n%!" (IPC_message.show msg);
            `Continue);
       IPC_client.wait_close c)

let () = main () |> Lwt_main.run
