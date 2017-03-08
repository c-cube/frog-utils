
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Client-Side} *)

module M = Lock_messages

open Frog

type remote_daemon = {
  port : int;
}

let section = Lwt_log.Section.make "LockClient"

let daemon_ch port f =
  Lwt_log.ign_debug_f ~section "trying to connect to daemon on port %d..." port;
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  Lwt_io.with_connection addr (fun (ic,oc) -> f ic oc)

let connect port f =
  daemon_ch port (fun _ oc ->
      Lwt_log.ign_debug_f ~section "connected to daemon";
      let%lwt () = M.print oc M.Start in
      let%lwt res = f {port; } in
      let%lwt () = M.print oc M.End in
      Lwt_log.ign_debug ~section "connection to daemon closed";
      Lwt.return res
    )

(* given the channels to the daemon, acquire lock, call [f], release lock *)
let acquire ?cwd ?user ?info ?(cores=0) ?(priority=1) ?(tags=[]) {port; } f =
  Lwt_log.ign_debug "acquiring lock...";
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  Lwt_io.with_connection addr
    (fun (ic,oc) ->
       let query_time = Unix.gettimeofday() in
       (* send "acquire" *)
       let pid = Unix.getpid() in
       let msg = M.Acquire {M.info; user; priority; query_time; tags; cwd; pid; cores} in
       let%lwt () = M.print oc msg in
       (* expect "go" *)
       let%lwt res = M.parse ic in
       match res with
       | M.Reject ->
         Lwt_log.ign_debug ~section "lock: rejected (daemon too busy?)";
         f false
       | M.Go ->
         Lwt_log.ign_debug ~section "acquired lock";
         let%lwt res = f true in
         Lwt_log.ign_debug ~section "release lock";
         let%lwt () = M.print oc M.Release in
         Lwt.return res
       | _ ->
         Lwt_log.ign_error_f "unexpected message: %s" (M.show res);
         Lwt.fail (M.Unexpected res)
    )

(* try to connect; if it fails, spawn daemon and retry *)
let connect_or_spawn ?(retry=1.) port f =
  try%lwt
    connect port f
  with _ ->
    (* launch daemon and re-connect *)
    Lwt_log.ign_info ~section "could not connect; launch daemon...";
    match%lwt Lock_daemon.fork_and_spawn port with
    | `child thread ->
        let%lwt () = thread in
        Lwt.fail Exit
    | `parent ->
        let%lwt () = Lwt_unix.sleep retry in
        Lwt_log.ign_info ~section "retry to connect to daemon...";
        connect port f

let connect_and_acquire
    ?cwd ?user ?info ?cores ?priority ?tags ?retry port f =
  connect_or_spawn ?retry port
    (fun conn ->
       acquire ?cwd ?user ?info ?cores ?priority ?tags conn f)

let get_status port =
  try%lwt
    daemon_ch port
      (fun ic oc ->
        let%lwt () = M.print oc M.Status in
        let%lwt ret = M.parse ic in
        let%lwt () = M.print oc M.StatusOk in
        match ret with
        | M.StatusAnswer ans ->
            Lwt.return (Some ans)
        | m ->
          Lwt.fail (M.Unexpected m)
      )
  with e ->
    Lwt_log.ign_debug_f ~section "encountered error : %s" (Printexc.to_string e);
    Lwt.return_none

(* connect to daemon (if any) and tell it to stop *)
let stop_accepting port =
  try%lwt
    daemon_ch port (fun _ oc -> M.print oc M.StopAccepting)
  with e ->
    Lwt_log.ign_error_f ~section "error: %s" (Printexc.to_string e);
    Lwt.return_unit


