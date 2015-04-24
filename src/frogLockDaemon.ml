
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

let job_id = M.new_id

let main_config_file = "/etc/froglock.conf"
let section = Lwt_log.Section.make "FrogLockDaemon"

type acquire_task = {
  query : M.job;
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
}

module Q = Heap.Make(struct
    type t = acquire_task
    let leq t t' =
      M.(t.query.priority > t'.query.priority) ||
      M.(t.query.priority = t'.query.priority && t.query.cores < t'.query.cores)
  end)

(* internal message between client handlers and the scheduler *)
type scheduler_msg =
  [ `Register of acquire_task
  | `Done of M.id
  ]

type state = {
  mutable max_cores : int;
  mutable num_clients : int;
  mutable accept : bool;
  mutable current : M.current_job list;
  mutable queue : Q.t;
  scheduler : scheduler_msg Lwt_mvar.t;
}

let make_state config_files =
  let config = FrogConfig.parse_files config_files
    (FrogConfig.parse_or_empty main_config_file) in
  {
  max_cores = FrogConfig.get_int ~default:1 config "cores";
  num_clients = 0;
  accept = true;
  current = [];
  queue = Q.empty;
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

let used_cores st =
  List.fold_left (fun cores job ->
      let j = M.(job.current_job.cores) in
      cores + (if j <= 0 then st.max_cores else j)) 0 st.current

let cores_needed st =
  match Q.find_min st.queue with
  | None -> 0
  | Some t ->
    let j = t.query.M.cores in
    if j <= 0 then st.max_cores else j

(* scheduler: receives requests from several clients, and pings them back *)
let start_scheduler ~state () =
  let inbox = state.scheduler in
  (* listen for new messages. [task] is the current running task, if any *)
  let rec listen () =
    let%lwt res = Lwt_mvar.take inbox in
    match res with
    | `Register task' ->
      Lwt_log.ign_info_f ~section "added query %d to queue" (task'.query.M.id :> int);
      push_task task' state;
      run_next ()
    | `Done id ->
      match List.partition (fun t -> t.M.current_job.M.id = id) state.current with
      | t :: _, l ->
        (* task is finished, run the next one *)
        Lwt_log.ign_info_f ~section "task %d finished (pid %d) after %.2fs"
          (id: M.id :> int) t.M.current_job.M.pid
          (Unix.gettimeofday() -. t.M.current_start);
        state.current <- l;
        run_next ()
      | [], _ ->
        Lwt_log.ign_error_f ~section "scheduler: unexpected 'Done' for task %d" (id : M.id :> int);
        listen ()
  (* run task *)
  and run_next () =
    if cores_needed state <= state.max_cores - used_cores state then
      if Q.is_empty state.queue then
        if used_cores state = 0 && state.num_clients = 0 then (
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
        let task = take_task state in
        Lwt_log.ign_info_f ~section "start task %d (user %s, pid %d): %s"
          (task.query.M.id : M.id :> int)
          (maybe_str task.query.M.user)
          task.query.M.pid
          (maybe_str task.query.M.info);
        let cur = {
          M.current_job=task.query;
          current_start=Unix.gettimeofday();
        } in
        state.current <- cur :: state.current;
        let%lwt () = M.print task.oc (M.Go task.query.M.id) in
        run_next ()
      )
    else
      listen ()
  in
  listen ()

(*
let handle_acquire ~state id (ic,oc) query =
  (* acquire lock *)
  let%lwt () = Lwt_mvar.take task.box in
  let release_ () =
    (* release lock *)
  in
  try%lwt
    (* start task *)
    Lwt_log.ign_debug_f ~section "task %d: send 'go'" id;
    let%lwt () = M.print oc M.Go in
    let%lwt _ = M.expect ic is_release_msg in
    release_ ()
  with _ ->
    release_ ()
*)

let handle_status ~state oc =
  let module M = M in
  let waiting = Q.fold
    (fun acc task -> task.query :: acc) [] state.queue
  in
  let waiting = List.rev waiting in
  let current = state.current in
  let ans = M.StatusAnswer {M.waiting; current} in
  M.print oc ans

let handle_msg ~state ic oc = function
  | M.Acquire q when not state.accept ->
    Lwt_log.ign_info ~section "ignore query (not accepting)";
    M.print oc (M.Reject q.M.id)
  | M.Acquire query ->
    Lwt_log.ign_info_f ~section "received new query (id %d)" (query.M.id :> int);
    let task = {query; ic; oc} in
    Lwt_mvar.put state.scheduler (`Register task)
  | M.Release id ->
    Lwt_log.ign_debug_f ~section "task %d: released" (id :> int);
    Lwt_mvar.put state.scheduler (`Done id)
  | M.Status ->
    Lwt_log.ign_info ~section "replying with status";
    handle_status ~state oc
  | M.StopAccepting ->
    Lwt_log.ign_info ~section "stop accepting jobs...";
    state.accept <- false;
    Lwt.return_unit
  | ( M.Start | M.End
    | M.StatusAnswer _
    | M.Go _ | M.Reject _
    ) as msg ->
    Lwt_log.ign_error "unexpected message received, :(";
    Lwt.fail (M.Unexpected msg)

(* handle one client.
  [cond_stop] condition to stop the server
  [ic,oc] connection to client *)
let rec handle_client ~state ic oc =
  let%lwt res = M.parse ic in
  match res with
  | M.Start ->
    state.num_clients <- state.num_clients + 1;
    Lwt_log.ign_info_f ~section "new connection (%d total)" state.num_clients;
    handle_client ~state ic oc
  | M.End ->
    state.num_clients <- state.num_clients - 1;
    Lwt_log.ign_info_f ~section "closed connection (%d total)" state.num_clients;
    Lwt.return_unit
  | msg ->
    let%lwt () = handle_msg ~state ic oc msg in
    handle_client ~state ic oc

(* spawn a daemon, to listen on the given port *)
let spawn port =
  Lwt_log.ign_info ~section "---------------------------";
  Lwt_log.ign_info_f ~section "starting daemon on port %d" port;
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let state = make_state [] in
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

let setup_loggers ?log_file () =
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
    Lwt_log.Section.set_level section Lwt_log.Debug;
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
