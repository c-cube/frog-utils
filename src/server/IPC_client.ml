
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
  on_next: msg Signal.t; (* call on incoming messages *)
  listen_thread: unit Lwt.t; (* loops on incoming messages *)
}

let section = Lwt_log.Section.make "LockClient"

let with_chans port f =
  Lwt_log.ign_debug_f ~section "trying to connect to daemon on port %d..." port;
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  Lwt_io.with_connection addr (fun (ic,oc) -> f ic oc)

let rec listen_loop (c:t): unit Lwt.t =
  let%lwt m = M.parse c.ic in
  Signal.send c.on_next m;
  listen_loop c

let wait_close (c:t): unit Lwt.t = c.listen_thread

let connect port f =
  with_chans port (fun ic oc ->
      Lwt_log.ign_debug_f ~section "connected to daemon";
      let%lwt () = M.print oc M.Start in
      let on_next = Signal.create() in
      (* loop on incoming messages *)
      let rec listen_loop () =
        let%lwt m = M.parse ic in
        Signal.send on_next m;
        listen_loop ()
      in
      let c = {
        port; ic; oc; on_next; listen_thread=listen_loop ();
      } in
      let%lwt res = f c in
      let%lwt () = M.print c.oc M.End in
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
  let msg = M.Acquire {M.info; user; priority; query_time; tags; cwd; pid; cores} in
  let%lwt () = M.print c.oc msg in
  (* expect "go" *)
  let%lwt res =
    next_filter c (function M.Go | M.Reject -> true | _ -> false)
  in
  begin match res with
    | M.Reject ->
      Lwt_log.ign_debug ~section "lock: rejected (daemon too busy?)";
      f false
    | M.Go ->
      Lwt_log.ign_debug ~section "acquired lock";
      let%lwt res = f true in
      Lwt_log.ign_debug ~section "release lock";
      let%lwt () = M.print c.oc M.Release in
      Lwt.return res
    | _ -> assert false
  end

(* try to connect; if it fails, spawn daemon and retry *)
let connect_or_spawn ?(retry=1.) port f =
  try%lwt
    connect port f
  with _ ->
    (* launch daemon and re-connect *)
    Lwt_log.ign_info ~section "could not connect; launch daemon...";
    begin match%lwt IPC_daemon.fork_and_spawn port with
    | `child thread ->
        let%lwt () = thread in
        Lwt.fail Exit
    | `parent ->
        let%lwt () = Lwt_unix.sleep retry in
        Lwt_log.ign_info ~section "retry to connect to daemon...";
        connect port f
    end

let connect_and_acquire
    ?cwd ?user ?info ?cores ?priority ?tags ?retry port f =
  connect_or_spawn ?retry port
    (fun conn ->
       acquire ?cwd ?user ?info ?cores ?priority ?tags conn
         (fun bool -> f (conn,bool)))

let send (c:t) msg = M.print c.oc msg

let on_msg (c:t) f: unit =
  Signal.on c.on_next
    (fun msg -> match f msg with
       | `Continue -> Signal.ContinueListening
       | `Stop -> Signal.StopListening)

let get_status (c:t): IPC_message.status_answer Lwt.t =
  let%lwt () = M.print c.oc M.Status in
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


