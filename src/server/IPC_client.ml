
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Client-Side} *)

open Frog

module M = IPC_message
module E = Misc.Err

type msg = M.t

type t = {
  port : int;
  ic: Lwt_io.input_channel;
  oc: Lwt_io.output_channel;
  mutable uid: int; (* query counter *)
  on_next: msg Signal.t; (* call on incoming messages *)
  listen_thread: unit Lwt.t; (* loops on incoming messages *)
}

let section = Lwt_log.Section.make "ipc_client"

let with_chans port f =
  Lwt_log.ign_debug_f ~section "trying to connect to daemon on port %d..." port;
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  Lwt_io.with_connection addr (fun (ic,oc) -> f ic oc)

let rec listen_loop (c:t): unit Lwt.t =
  let%lwt m = M.parse c.ic in
  Signal.send c.on_next m;
  listen_loop c

let wait_close (c:t): unit Lwt.t = c.listen_thread

let send (c:t) msg = M.print c.oc msg

let send_noerr c msg =
  try%lwt
    send c msg
  with e ->
    Lwt_log.error_f "could not send message %s:\n%s"
      (IPC_message.show msg) (Printexc.to_string e)

let connect port f =
  with_chans port
    (fun ic oc ->
      Lwt_log.ign_debug_f ~section "connected to daemon";
      let%lwt () = M.print oc M.Start in
      let on_next = Signal.create() in
      (* loop on incoming messages *)
      let rec listen_loop () =
        let%lwt m = M.parse ic in
        (* answer to "ping" *)
        begin match m with
          | M.Ping n ->
            Lwt.async
              (fun () ->
                 try%lwt M.print oc (M.Pong n)
                 with _ -> Lwt.return_unit)
          | _ -> ()
        end;
        Signal.send on_next m;
        listen_loop ()
      in
      let c = {
        port; ic; oc; on_next; uid=0; listen_thread=listen_loop ();
      } in
      let%lwt res = f c in
      Lwt_log.ign_debug ~section "send `end` to daemon";
      let%lwt () = send_noerr c M.End in
      Lwt.cancel c.listen_thread;
      Lwt_log.ign_debug ~section "connection to daemon closed";
      Lwt.return res
    )

(* wait for the next message satisfying the predicate *)
let next_filter_map (c:t) (f:msg->'a option): 'a Lwt.t =
  let reply, send_reply = Lwt.wait () in
  Signal.on c.on_next
    (fun m -> match f m with
       | None -> Signal.ContinueListening
       | Some x ->
         Lwt.wakeup send_reply x;
         Signal.StopListening);
  reply

(* wait for the next message satisfying the predicate *)
let next_filter (c:t) (f:msg->bool): msg Lwt.t =
  let reply, send_reply = Lwt.wait () in
  Signal.on c.on_next
    (fun m ->
       if f m then (
         Lwt.wakeup send_reply m;
         Signal.StopListening
       ) else Signal.ContinueListening);
  reply

(* given the channels to the daemon, acquire lock, call [f], release lock *)
let acquire ?cwd ?user ?info ?(cores=0) ?(priority=1) ?(tags=[]) (c:t) f =
  let query_time = Unix.gettimeofday() in
  (* send "acquire" *)
  let pid = Unix.getpid() in
  let uid = c.uid in
  c.uid <- uid+1;
  let msg =
    M.Acquire {M.info; uid; user; priority; query_time;
               tags; cwd; pid; cores} in
  let%lwt () = send_noerr c msg in
  (* expect "go" or "reject" for this uid *)
  let%lwt res =
    next_filter c (function (M.Go u | M.Reject u) -> u=uid | _ -> false)
  in
  begin match res with
    | M.Reject u ->
      Lwt_log.ign_debug ~section "lock: rejected (daemon too busy or stopped?)";
      assert (u=uid);
      f false
    | M.Go u ->
      Lwt_log.ign_debug ~section "acquired lock";
      Lwt.finalize
        (fun () ->
           assert (u=uid);
           f true)
        (fun () ->
          Lwt_log.ign_debug ~section "release lock";
          send_noerr c (M.Release uid))
    | _ -> assert false
  end

(* try to connect; if it fails, spawn daemon and retry *)
let connect_or_spawn ?(retry=1.) port f =
  try%lwt
    connect port f
  with _ ->
    (* launch daemon and re-connect *)
    Lwt_log.ign_info ~section "could not connect; launch daemon...";
    IPC_daemon.fork_daemon port;
    let%lwt () = Lwt_unix.sleep retry in
    Lwt_log.ign_info ~section "retry to connect to daemon...";
    connect port f

let connect_and_acquire
    ?cwd ?user ?info ?cores ?priority ?tags ?retry port f =
  connect_or_spawn ?retry port
    (fun conn ->
       acquire ?cwd ?user ?info ?cores ?priority ?tags conn
         (fun bool -> f (conn,bool)))

let on_msg (c:t) f: unit =
  Signal.on c.on_next
    (fun msg -> match f msg with
       | `Continue -> Signal.ContinueListening
       | `Stop -> Signal.StopListening)

let get_status (c:t): IPC_message.status_answer Lwt.t =
  let%lwt () = send_noerr c M.Status in
  let%lwt res =
    next_filter_map c (function M.StatusAnswer ans -> Some ans | _ -> None)
  in
  Lwt.return res

let connect_and_get_status port =
  let open Lwt.Infix in
  try%lwt
    connect port get_status >|= CCOpt.return
  with e ->
    Lwt_log.ign_debug_f ~section "encountered error : %s" (Printexc.to_string e);
    Lwt.return_none

(* connect to daemon (if any) and tell it to stop *)
let stop_accepting_jobs port =
  try%lwt
    with_chans port (fun _ oc -> M.print oc M.StopAccepting)
  with e ->
    Lwt_log.ign_error_f ~section "error: %s" (Printexc.to_string e);
    Lwt.return_unit


