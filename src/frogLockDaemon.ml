
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Daemon} *)

module M = FrogLockMessages

let main_config_file = "/etc/froglock.conf"
let section = Logs.Src.create "FrogLockDaemon"

type acquire_task = {
  id : int;
  query : M.job;
  ic : in_channel ;
  oc : out_channel;
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
  mutable queue : Q.t CCLock.t;
  scheduler : scheduler_msg CCThread.Queue.t;
}

let make_state config_files =
  let config = FrogConfig.parse_files config_files
    (FrogConfig.parse_or_empty main_config_file) in
  {
    current_id = 0;
    max_cores = FrogConfig.get_int ~default:1 config "cores";
    num_clients = 0;
    accept = true;
    current = [];
    queue = CCLock.create Q.empty;
    scheduler = CCThread.Queue.create 1;
  }

let push_task t st =
  CCLock.update st.queue (fun q -> Q.add q t)

let take_task st =
  CCLock.with_lock_as_ref st.queue
    ~f:(fun l ->
      let q = CCLock.LockRef.get l in
      let q', t = Q.take_exn q in
      CCLock.LockRef.set l q';
      t)

let maybe_str = function
  | None -> "<none>"
  | Some s -> s

let used_cores st =
  List.fold_left (fun cores job ->
      let j = M.(job.current_job.cores) in
      cores + (if j <= 0 then st.max_cores else j)) 0 st.current

let cores_needed st =
  match Q.find_min (CCLock.get st.queue) with
  | None -> 0
  | Some t ->
    let j = t.query.M.cores in
    if j <= 0 then st.max_cores else j

let log_state state =
  Logs.info ~src:section
    (fun k->k
      "current state : %d running, %d / %d cores, %d in queue, %d active connections"
      (List.length state.current) (used_cores state) (state.max_cores)
      (Q.size (CCLock.get state.queue)) state.num_clients)

(* scheduler: receives requests from several clients, and pings them back *)
let start_scheduler ~state () =
  let inbox = state.scheduler in
  (* listen for new messages. [task] is the current running task, if any *)
  let rec listen () =
    let res = CCThread.Queue.take inbox in
    match res with
    | `Register task' ->
      Logs.info ~src:section (fun k->k "added query %d to queue" task'.id);
      push_task task' state;
      run_next ()
    | `Refresh ->
      run_next ()
    | `Done id ->
      match List.partition (fun t -> t.M.current_id = id) state.current with
      | t :: _, l ->
        (* task is finished, run the next one *)
        Logs.info ~src:section
          (fun k->k "task %d finished (pid %d) after %.2fs"
          id t.M.current_job.M.pid
          (Unix.gettimeofday() -. t.M.current_start));
        state.current <- l;
        run_next ()
      | [], _ ->
        Logs.err ~src:section (fun k->k "scheduler: unexpected 'Done' for task %d" id);
        run_next ()
  (* run task *)
  and run_next () =
    log_state state;
    if cores_needed state <= state.max_cores - used_cores state then
      if Q.is_empty (CCLock.get state.queue) then
        if used_cores state = 0 && state.num_clients = 0 then (
          (* only exit if no clients are connected, to avoid the
             race condition:
              - client connects
              - queue is empty --> scheduler stops
              - client sends "acquire" and never gets an answer *)
          Logs.info ~src:section (fun k->k "no more tasks nor clients, exit");
          ()
        ) else listen ()
      else (
        (* start the given process *)
        let task = take_task state in
        Logs.info ~src:section
          (fun k->k "start task %d (user %s, pid %d): %s"
          task.id
          (maybe_str task.query.M.user)
          task.query.M.pid
          (maybe_str task.query.M.info));
        let cur = {
          M.current_id = task.id;
          current_job = task.query;
          current_start = Unix.gettimeofday ();
        } in
        state.current <- cur :: state.current;
        M.print task.oc M.Go;
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
  Logs.info ~src:section (fun k->k "replying with status");
  let waiting =
    CCLock.with_lock state.queue
      (Q.fold
        (fun acc task ->
           {M.waiting_id = task.id; waiting_job = task.query } :: acc)
        [])
  in
  let waiting = List.rev waiting in
  let current = state.current in
  let ans = M.StatusAnswer {M.waiting; current; max_cores = state.max_cores} in
  M.print oc ans

let handle_query ~state ic oc query =
  let id = state.current_id in
  state.current_id <- state.current_id + 1;
  Logs.info ~src:section (fun k->k "received new query (id %d)" id);
  let task = {id; query; ic; oc} in
  CCThread.Queue.push state.scheduler (`Register task);
  let msg = M.parse ic in
  begin match msg with
    | M.Release -> ()
    | _ ->
        Logs.err ~src:section (fun k->k "unexpected message: %a" M.pp msg)
  end;
  Logs.debug ~src:section (fun k->k "task %d: released" id);
  CCThread.Queue.push state.scheduler (`Done id)

let handle_keepalive ~state ic oc =
  if state.accept then (
    state.num_clients <- state.num_clients + 1;
    Logs.info ~src:section (fun k->k "new connection (%d total)" state.num_clients);
    let _ = M.expect ic ((=) M.End) in
    state.num_clients <- state.num_clients - 1;
    Logs.info ~src:section (fun k->k "closed connection (%d total)" state.num_clients);
    CCThread.Queue.push state.scheduler `Refresh;
  ) else (
    Logs.info ~src:section (fun k->k "ignore query (not accepting)");
    M.print oc M.Reject
  )

(* handle one client.
  [cond_stop] condition to stop the server
  [ic,oc] connection to client *)
let handle_client ~state ic oc =
  let res = M.parse ic in
  match res with
  | M.Status -> handle_status ~state oc
  | M.Start -> handle_keepalive ~state ic oc
  | M.Acquire query -> handle_query ~state ic oc query
  | M.StopAccepting ->
    Logs.info ~src:section (fun k->k "stop accepting jobs...");
    state.accept <- false;
  | ( M.StatusAnswer _
    | M.Go | M.Reject
    | M.End | M.Release
    ) as msg ->
    raise (M.Unexpected msg)

(* spawn a daemon, to listen on the given port *)
let spawn port =
  Logs.info ~src:section (fun k->k "---------------------------");
  Logs.info ~src:section (fun k->k "starting daemon on port %d" port);
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let state = make_state [] in
  (* scheduler *)
  Logs.info ~src:section (fun k->k "start scheduler");
  let _run_scheduler = CCThread.spawn (start_scheduler ~state) in
  Logs.info ~src:section (fun k->k "scheduler started");
  (* server that listens for incoming clients *)
  Logs.debug ~src:section (fun k->k "daemon started");
  CCUnix.establish_server addr
    ~f:(fun ic oc -> CCThread.detach (fun () -> handle_client ~state ic oc));
  ()

(* TODO: change log level through connection *)

(* TODO: register a reporter *)
let setup_loggers ?log_file:_ () =
  ()
  (* FIXME syslog?
  let syslog = Lwt_log.syslog ~facility:`User () in
  Lwt_log.default := syslog;
  begin match log_file with
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
  *)

(* fork and spawn a daemon on the given port *)
let fork_and_spawn ?log_file port =
  match Unix.fork () with
  | 0 -> (* child: double fork to avoid zombies  *)
    begin match Unix.fork () with
    | 0 -> (* grand child, do the work *)
        Unix.chdir "/tmp";
        Unix.close Unix.stdin;
        Unix.close Unix.stdout;
        setup_loggers ?log_file ();
        (* FIXME
           Lwt_log.Section.set_level section Lwt_log.Debug; *)
        Logs.debug ~src:section (fun k->k "loggers are setup");
        let thread =
          try CCThread.spawn (fun () -> spawn port)
          with e ->
            Logs.err ~src:section
              (fun k->k "daemon: error: %s" (Printexc.to_string e));
            raise e
        in
        `child thread
    | _ -> exit 0 (* child, do nothing *)
    end
  | _ -> `parent
