
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

let section = Lwt_log.Section.make "FrogLockDaemon"

type acquire_task = {
  box : unit Lwt_mvar.t;
  id : int;
  query : M.job;
}

module Q = Heap.Make(struct
    type t = acquire_task
    let leq t t' = M.(t.query.priority > t'.query.priority)
  end)

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
  mutable queue : Q.t;
  scheduler : scheduler_msg Lwt_mvar.t;
}

let make_state () = {
  num_clients=0;
  cur_id=0;
  accept=true;
  current= None;
  queue= Q.empty;
  scheduler = Lwt_mvar.create_empty ();
}

let push_task t st = st.queue <- Q.add st.queue t

let take_task st =
  let q, t = Q.take_exn st.queue in
  st.queue <- q;
  t

let maybe_str = function
  | None -> "<none>"
  | Some s -> s

(* scheduler: receives requests from several clients, and pings them back *)
let start_scheduler ~state () =
  let inbox = state.scheduler in
  (* listen for new messages. [task] is the current running task, if any *)
  let rec listen () =
    let%lwt res = Lwt_mvar.take inbox in
    match res with
    | `Register task' ->
        push_task task' state;
        begin match state.current with
        | None -> run_next ()
        | Some _ -> listen ()
        end
    | `Done task' ->
        begin match state.current with
        | Some t when t.M.current_id = task'.id ->
            (* task if finished, run the next one *)
            Lwt_log.ign_info_f ~section "task %d finished (pid %d) after %.2fs"
              task'.id t.M.current_job.M.pid
              (Unix.gettimeofday() -. t.M.current_start);
            state.current <- None;
            run_next ()
        | _ ->
          Lwt_log.ign_error_f ~section "scheduler: unexpected 'Done' for task %d" task'.id;
          listen ()
        end
  (* run task *)
  and run_next () =
    if Q.is_empty state.queue
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
      let task = take_task state in
      Lwt_log.ign_info_f ~section "start task %d (user %s, pid %d): %s"
        task.id
        (maybe_str task.query.M.user)
        task.query.M.pid
        (maybe_str task.query.M.info);
      let cur = {
        M.current_job=task.query;
        current_id=task.id;
        current_start=Unix.gettimeofday();
      } in
      state.current <- Some cur;
      let%lwt () = Lwt_mvar.put task.box () in
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
  let%lwt () = Lwt_mvar.put state.scheduler (`Register task) in
  let%lwt () = Lwt_mvar.take task.box in
  let release_ () =
    (* release lock *)
    Lwt_log.ign_debug_f ~section "task %d: released" id;
    state.num_clients <- state.num_clients - 1;
    Lwt_mvar.put state.scheduler (`Done task)
  in
  try%lwt
    (* start task *)
    Lwt_log.ign_debug_f ~section "task %d: send 'go'" id;
    let%lwt () = M.print oc M.Go in
    let%lwt _ = M.expect ic is_release_msg in
    release_ ()
  with _ ->
    release_ ()

let stop_accepting ~state =
  Lwt_log.ign_info ~section "stop accepting jobs...";
  state.accept <- false;
  Lwt.return_unit

let handle_status ~state oc =
  let module M = M in
  let waiting = Q.fold
    (fun acc task ->
      { M.waiting_id=task.id; waiting_job=task.query; } :: acc
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
  let%lwt res = M.parse ic in
  match res with
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
  let%lwt () = run_scheduler in
  Lwt_log.ign_debug ~section "daemon's server is stopping";
  Lwt_io.shutdown_server server;
  Lwt.return_unit

(* TODO: change log level through connection *)

let setup_loggers  ?log_file () =
  let syslog = Lwt_log.syslog ~facility:`User () in
  Lwt_log.default := syslog;
  let%lwt () =  match log_file with
    | None -> Lwt.return_unit
    | Some file_name ->
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

(* fork and spawn a daemon on the given port *)
let fork_and_spawn ?log_file port =
  match Lwt_unix.fork () with
  | 0 -> (* child, will be the daemon *)
    Lwt_daemon.daemonize ~syslog:false ~directory:"/tmp"
      ~stdin:`Close ~stdout:`Close ~stderr:`Keep ();
    let%lwt () = setup_loggers ?log_file () in
    Lwt_log.Section.set_level section Lwt_log.Info;
    Lwt_log.ign_debug ~section "loggers are setup";
    let thread =
      try%lwt
        spawn port
      with e ->
        Lwt_log.ign_error_f ~section "daemon: error: %s" (Printexc.to_string e);
        Lwt.return_unit
    in
    Lwt.return (`child thread)
  | _ -> Lwt.return `parent
