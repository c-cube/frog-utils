
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Daemon} *)

open Frog

module M = Lock_messages

let main_config_file = "/etc/froglock.conf"
let section = Lwt_log.Section.make "LockDaemon"

type acquire_task = {
  id : int;
  query : M.job;
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
}

module Q = CCHeap.Make(struct
    type t = acquire_task
    let leq t t' =
      M.(t.query.priority > t'.query.priority) ||
      M.(t.query.priority = t'.query.priority && t.query.cores < t'.query.cores)
  end)

(* internal message between client handlers and the scheduler *)
type scheduler_msg =
  [ `Register of acquire_task
  | `Done of int
  | `Refresh
  ]

type state = {
  mutable current_id : int;
  mutable max_cores : int;
  mutable num_clients : int;
  mutable accept : bool;
  mutable current : M.current_job list;
  mutable queue : Q.t;
  scheduler : scheduler_msg Lwt_mvar.t;
}

let make_state config_files =
  let config = Config.parse_files config_files
    (Config.parse_or_empty main_config_file) in
  {
    current_id = 0;
    max_cores = Config.get_int ~default:1 config "cores";
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

let log_state state =
  Lwt_log.ign_info_f ~section "current state : %d running, %d / %d cores, %d in queue, %d active connections"
    (List.length state.current) (used_cores state) (state.max_cores) (Q.size state.queue) state.num_clients

(* scheduler: receives requests from several clients, and pings them back *)
let start_scheduler ~state () =
  let inbox = state.scheduler in
  (* listen for new messages. [task] is the current running task, if any *)
  let rec listen () =
    let%lwt res = Lwt_mvar.take inbox in
    match res with
    | `Register task' ->
      Lwt_log.ign_info_f ~section "added query %d to queue" task'.id;
      push_task task' state;
      run_next ()
    | `Refresh ->
      run_next ()
    | `Done id ->
      match List.partition (fun t -> t.M.current_id = id) state.current with
      | t :: _, l ->
        (* task is finished, run the next one *)
        Lwt_log.ign_info_f ~section "task %d finished (pid %d) after %.2fs"
          id t.M.current_job.M.pid
          (Unix.gettimeofday() -. t.M.current_start);
        state.current <- l;
        run_next ()
      | [], _ ->
        Lwt_log.ign_error_f ~section "scheduler: unexpected 'Done' for task %d" id;
        run_next ()
  (* run task *)
  and run_next () =
    log_state state;
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
          task.id
          (maybe_str task.query.M.user)
          task.query.M.pid
          (maybe_str task.query.M.info);
        let cur = {
          M.current_id = task.id;
          current_job = task.query;
          current_start = Unix.gettimeofday ();
        } in
        state.current <- cur :: state.current;
        let%lwt () = M.print task.oc M.Go in
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

let handle_status ~state ic oc =
  let module M = M in
  Lwt_log.ign_info ~section "replying with status";
  let waiting = Q.fold
      (fun acc task ->
         {M.waiting_id = task.id; waiting_job = task.query } :: acc
      ) [] state.queue
  in
  let waiting = List.rev waiting in
  let current = state.current in
  let ans = M.StatusAnswer {M.waiting; current; max_cores = state.max_cores} in
  let%lwt () = M.print oc ans in
  let%lwt _ = M.expect ic ((=) M.StatusOk) in
  Lwt.return_unit

let handle_query ~state ic oc query =
  let id = state.current_id in
  state.current_id <- state.current_id + 1;
  Lwt_log.ign_info_f ~section "received new query (id %d)" id;
  let task = {id; query; ic; oc} in
  let%lwt () = Lwt_mvar.put state.scheduler (`Register task) in
  let%lwt msg = M.parse ic in
  begin match msg with
    | M.Release -> ()
    | _ -> Lwt_log.ign_error_f "unexpected message: %s" (M.show msg)
  end;
  Lwt_log.ign_debug_f ~section "task %d: released" id;
  Lwt_mvar.put state.scheduler (`Done id)

let handle_keepalive ~state ic oc =
  if state.accept then begin
    state.num_clients <- state.num_clients + 1;
    Lwt_log.ign_info_f ~section "new connection (%d total)" state.num_clients;
    let%lwt _ = M.expect ic ((=) M.End) in
    state.num_clients <- state.num_clients - 1;
    Lwt_log.ign_info_f ~section "closed connection (%d total)" state.num_clients;
    Lwt_mvar.put state.scheduler `Refresh
  end else begin
    Lwt_log.ign_info ~section "ignore query (not accepting)";
    M.print oc M.Reject
  end

(* handle one client.
  [cond_stop] condition to stop the server
  [ic,oc] connection to client *)
let handle_client ~state ic oc =
  let%lwt res = M.parse ic in
  match res with
  | M.Status -> handle_status ~state ic oc
  | M.Start -> handle_keepalive ~state ic oc
  | M.Acquire query -> handle_query ~state ic oc query
  | M.StopAccepting ->
    Lwt_log.ign_info ~section "stop accepting jobs...";
    state.accept <- false;
    Lwt.return_unit
  | ( M.StatusAnswer _
    | M.StatusOk
    | M.Go | M.Reject
    | M.End | M.Release
    ) as msg ->
    Lwt.fail (M.Unexpected msg)

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
      Lwt.async
        (fun () ->
           Lwt.finalize
             (fun () -> handle_client ~state ic oc)
             (fun () ->
               let%lwt () = Lwt_io.close ic in
               Lwt_io.close oc))
    )
  in
  (* stop *)
  Lwt_log.ign_debug ~section "daemon started";
  let%lwt () = run_scheduler in
  Lwt_log.ign_debug ~section "daemon's server is stopping";
  Lwt_io.shutdown_server server;
  Lwt.return_unit

(* TODO: change log level through connection *)

let setup_loggers file_name () =
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

(* fork and spawn a daemon on the given port *)
let fork_and_spawn port =
  match Lwt_unix.fork () with
  | 0 -> (* child, will be the daemon *)
    Lwt_daemon.daemonize ~syslog:false ~directory:"/tmp"
      ~stdin:`Close ~stdout:`Close ~stderr:`Keep ();
    let log_file =
      let config = Config.parse_or_empty main_config_file in
      Config.get_string ~default:"/tmp/froglock.log" config "log"
    in
    let%lwt () = setup_loggers log_file () in
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

