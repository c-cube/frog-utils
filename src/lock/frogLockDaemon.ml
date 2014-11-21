
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Daemon} *)

module M = FrogLockMessages
let (>>=) = Lwt.(>>=)

let section = Lwt_log.Section.make "FrogLockDaemon"

type acquire_task = {
  box : unit Lwt_mvar.t;
  id : int;
  query : M.acquire_query;
}

(* internal message between client handlers and the scheduler *)
type scheduler_msg =
  [ `Register of acquire_task
  | `Done of acquire_task
  ]

type state = {
  mutable num_clients : int;
  mutable cur_id : int;
  mutable accept : bool;
  mutable current : M.current_job option;
  queue : acquire_task Queue.t;
  scheduler : scheduler_msg Lwt_mvar.t;
}

let make_state () = {
  num_clients=0;
  cur_id=0;
  accept=true;
  current= None;
  queue=Queue.create ();
  scheduler = Lwt_mvar.create_empty ();
}

(* scheduler: receives requests from several clients, and pings them back *)
let start_scheduler ~state () =
  let inbox = state.scheduler in
  (* listen for new messages. [task] is the current running task, if any *)
  let rec listen () =
    Lwt_mvar.take inbox >>= function
    | `Register task' ->
        Queue.push task' state.queue;
        begin match state.current with
        | None -> run_next ()
        | Some _ -> listen ()
        end
    | `Done task' ->
        begin match state.current with
        | Some t when t.M.current_id = task'.id ->
            (* task if finished, run the next one *)
            Lwt_log.ign_info_f ~section "task %d finished (pid %d) after %.2fs"
              task'.id t.M.current_pid
              (Unix.gettimeofday() -. t.M.current_start);
            state.current <- None;
            run_next ()
        | _ ->
          Lwt_log.ign_error_f ~section "scheduler: unexpected 'Done' for task %d" task'.id;
          listen ()
        end
  (* run task *)
  and run_next () =
    if Queue.is_empty state.queue
    then if state.num_clients = 0
      then (
        (* only exit if no clients are connected, to avoid the
            race condition:
              - client connects
              - queue is empty --> scheduler stops
              - client sends "acquire" and never gets an answer *)
        Lwt_log.ign_info ~section "no more tasks nor clients, exit";
        Lwt.return_unit
      ) else listen ()
    else (
      (* start the given process *)
      assert (state.current = None);
      let task = Queue.take state.queue in
      Lwt_log.ign_info_f ~section "start task %d (user %s, pid %d): %s"
        task.id
        ([%show:string option] task.query.M.user)
        task.query.M.pid
        ([%show:string option] task.query.M.info);
      let cur = {
        M.current_id=task.id;
        current_user=task.query.M.user;
        current_pid=task.query.M.pid;
        current_info=task.query.M.info;
        current_start=Unix.gettimeofday();
      } in
      state.current <- Some cur;
      Lwt_mvar.put task.box () >>= fun () ->
      listen ()
    )
  in
  listen ()

let is_release_msg = function
  | M.Release -> true
  | _ -> false

let handle_acquire ~state id (ic,oc) query =
  let task = {box=Lwt_mvar.create_empty (); id; query} in
  (* acquire lock *)
  Lwt_mvar.put state.scheduler (`Register task) >>= fun () ->
  Lwt_mvar.take task.box >>= fun () ->
  let release_ () =
    (* release lock *)
    Lwt_log.ign_debug_f ~section "task %d: released" id;
    state.num_clients <- state.num_clients - 1;
    Lwt_mvar.put state.scheduler (`Done task)
  in
  Lwt.catch
    (fun () ->
      (* start task *)
      Lwt_log.ign_debug_f ~section "task %d: send 'go'" id;
      M.print oc M.Go >>= fun () ->
      M.expect ic is_release_msg >>= fun _ ->
      release_ ()
    ) (fun _ -> release_ ())

let stop_accepting ~state =
  Lwt_log.ign_info ~section "stop accepting jobs...";
  state.accept <- false;
  Lwt.return_unit

let handle_status ~state oc =
  let module M = M in
  let waiting = Queue.fold
    (fun acc job ->
      {M.waiting_pid=job.query.M.pid;
       waiting_user=job.query.M.user;
       waiting_id=job.id;
       waiting_info=job.query.M.info} :: acc
    ) [] state.queue
  in
  let waiting = List.rev waiting in
  let current = state.current in
  let ans = M.StatusAnswer {M.waiting; current} in
  M.print oc ans

(* handle one client.
  [cond_stop] condition to stop the server
  [ic,oc] connection to client *)
let handle_client ~state ic oc =
  M.parse ic >>= function
  | M.Acquire _ when not state.accept ->
      Lwt_log.ign_info ~section "ignore query (not accepting)";
      M.print oc M.Reject
  | M.Acquire q ->
      let id = state.cur_id in
      state.cur_id <- state.cur_id + 1;
      Lwt_log.ign_info_f ~section "received new query (id %d)" id;
      state.num_clients <- state.num_clients + 1;
      handle_acquire ~state id (ic,oc) q
  | M.Status ->
      handle_status ~state oc
  | M.StopAccepting ->
      stop_accepting ~state
  | ( M.StatusAnswer _
    | M.Release
    | M.Go
    | M.Reject
    ) as msg ->
      Lwt.fail (M.Unexpected msg)

(* spawn a daemon, to listen on the given port *)
let spawn port =
  Lwt_log.ign_info_f ~section "starting daemon on port %d" port;
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let state = make_state () in
  (* scheduler *)
  Lwt_log.ign_info ~section "start scheduler";
  let run_scheduler = start_scheduler ~state () in
  Lwt_log.ign_info ~section "scheduler started";
  (* server that listens for incoming clients *)
  let server = Lwt_io.establish_server addr
    (fun (ic,oc) ->
      Lwt.async (fun () -> handle_client ~state ic oc)
    )
  in
  (* stop *)
  Lwt_log.ign_debug ~section "daemon started";
  run_scheduler >>= fun () ->
  Lwt_log.ign_debug ~section "daemon's server is stopping";
  Lwt_io.shutdown_server server;
  Lwt.return_unit

(* TODO: more general logging (param for the file; syslog)? *)
(* TODO: change log level through connection *)

(* fork and spawn a daemon on the given port *)
let fork_and_spawn port =
  match Lwt_unix.fork () with
  | 0 -> (* child, will be the daemon *)
    Lwt_daemon.daemonize ~syslog:false ~directory:"/tmp"
      ~stdin:`Close ~stdout:`Close ~stderr:`Close ();
    (* change logger *)
    Lwt_log.file ~mode:`Append ~perm:0o666 ~file_name:"/tmp/join_lock.log" ()
    >>= fun logger ->
    Lwt_log.default := logger;
    Lwt_log.add_rule "*" Lwt_log.Info;
    let thread = Lwt.catch
      (fun () -> spawn port)
      (fun e ->
        Lwt_log.ign_error_f "daemon: error: %s" (Printexc.to_string e);
        Lwt.return_unit
      )
    in
    Lwt.return (`child thread)
  | _ -> Lwt.return `parent
