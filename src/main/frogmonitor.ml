
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Monitor Events} *)

open Frog
open Frog_server

module E = Misc.LwtErr

let main () =
  let open E.Infix in
  Pub_sub.create () >>= fun pubsub ->
  Lwt_log.ign_debug_f "opened pubsub socket";
  (* read event, print it, repeat *)
  let rec loop () =
    Pub_sub.next pubsub >>= fun e ->
    Printf.printf "read: %s\n%!" (Pub_sub.show_msg e);
    loop ()
  in
  loop()

let () = main () |> E.to_exn |> Lwt_main.run
