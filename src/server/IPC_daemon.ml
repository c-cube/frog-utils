
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Daemon for Coordination} *)

open Frog
open Lwt.Infix

module M = IPC_message
module Int_map = Misc.Int_map

let main_config_file = "/etc/froglock.conf"
let section = Lwt_log.Section.make "ipc_daemon"

let default_port = 12_042

type uid = M.uid

(* a pair (client id, client-relative uid) *)
module Full_id = struct
  type t = {
    id_client: int;
    id_task: uid;
  }

  let make id_client id_task: t = {id_client; id_task}
  let compare (a:t) (b:t): int =
    let open CCOrd.Infix in
    CCOrd.int a.id_client b.id_client
    <?> (CCOrd.int, a.id_task, b.id_task)

  let to_string (a:t): string =
    Printf.sprintf "(:c_id %d :uid %d)" a.id_client a.id_task
  let pp out (a:t): unit = CCFormat.string out (to_string a)
end

module Full_id_map = CCMap.Make(Full_id)

(* connection to a client *)
type client_conn = {
  c_id: int; (* unique client id *)
  c_in: Lwt_io.input_channel;
  c_out: Lwt_io.output_channel;
  mutable c_last_ping: float;
  mutable c_last_pong: float;
  mutable c_active: active_task Full_id_map.t;
  mutable c_thread: unit Lwt.t;
}

(* a given "lock" task *)
and acquire_task = {
  t_query : M.job;
  t_conn: client_conn;
}

and active_task = {
  at_task: acquire_task;
  at_start: float;
}

let task_id (t:acquire_task): Full_id.t = Full_id.make t.t_conn.c_id t.t_query.M.uid
let active_id (at:active_task): Full_id.t = task_id at.at_task

module Q = CCHeap.Make(struct
    type t = acquire_task
    let leq t t' =
      M.(t.t_query.priority > t'.t_query.priority) ||
      M.(t.t_query.priority = t'.t_query.priority && t.t_query.cores < t'.t_query.cores) ||
      M.(t.t_query.priority = t'.t_query.priority &&
         t.t_query.cores = t'.t_query.cores &&
         ( task_id t < task_id t' ||
           (task_id t = task_id t' && t.t_conn.c_id < t'.t_conn.c_id) ))
  end)

(* message received by client *)
type event =
  | E_refresh (* check timeout and all that *)
  | E_recv of client_conn * M.t

module State : sig
  type t
  val make : forever:bool -> string list -> t

  val clients : t -> client_conn Int_map.t
  val num_clients : t -> int
  val add_client : t -> client_conn -> unit
  val remove_client : t -> client_conn -> unit
  val find_active : t -> Full_id.t -> active_task option
  val remove_active : t -> active_task -> unit
  val add_active : t -> active_task -> unit
  val num_active : t -> int
  val active_l : t -> active_task list
  val waiting : t -> Q.t
  val send_event : t -> event -> unit Lwt.t
  val next_event : t -> event Lwt.t
  val push_task : t -> acquire_task -> unit
  val peek_task : t -> acquire_task option
  val take_task : t -> acquire_task option
  val max_cores : t -> int
  val used_cores : t -> int
  val forever : t -> bool
  val cores_needed : t -> int
  val new_id: t -> int
  val accept : t -> bool
  val stop_accept : t -> unit
  val time_since_last_event: t -> float

  val log_state: t -> unit
end = struct
  type t = {
    mutable id_count : int; (* for allocating client IDs *)
    mutable msg_count : int; (* unique count for messages *)
    mutable max_cores : int;
    mutable accept : bool;
    mutable active : active_task Full_id_map.t;
    mutable clients : client_conn Int_map.t; (* c_id -> client *)
    mutable waiting : Q.t; (* for scheduling tasks *)
    mutable last_event: float;
    forever: bool;
    events : event Lwt_mvar.t;
  }

  let active t = t.active
  let add_active t a = t.active <- Full_id_map.add (active_id a) a t.active
  let remove_active t a = t.active <- Full_id_map.remove (active_id a) t.active
  let find_active t id = Full_id_map.get id t.active
  let num_active t = Full_id_map.cardinal t.active
  let active_l t = Full_id_map.to_list t.active |> List.rev_map snd
  let waiting t = t.waiting
  let forever t = t.forever
  let clients t = t.clients
  let time_since_last_event t = Unix.gettimeofday() -. t.last_event
  let num_clients t = Int_map.cardinal (clients t)
  let add_client t c = t.clients <- Int_map.add c.c_id c t.clients
  let remove_client t c = t.clients <- Int_map.remove c.c_id t.clients
  let max_cores t = t.max_cores
  let send_event (st:t) (e:event): unit Lwt.t =
    st.last_event <- Unix.gettimeofday();
    Lwt_mvar.put st.events e
  let next_event (st:t): event Lwt.t =
    let%lwt res = Lwt_mvar.take st.events in
    st.last_event <- Unix.gettimeofday();
    Lwt.return res
  let accept t = t.accept
  let stop_accept t = t.accept <- false
  let push_task st t = st.waiting <- Q.add st.waiting t
  let peek_task st = Q.find_min st.waiting
  let take_task st: acquire_task option =
    begin match Q.take st.waiting with
      | None -> None
      | Some (q,t) ->
        st.waiting <- q;
        Some t
    end

  let new_id st: int =
    let id = st.id_count in
    st.id_count <- st.id_count + 1;
    id

  let used_cores st: int =
    Full_id_map.fold
      (fun _ at cores ->
         let j = at.at_task.t_query.M.cores in
         cores + (if j <= 0 then max_cores st else j))
      st.active 0

  let cores_needed st: int = match peek_task st with
    | None -> 0
    | Some t ->
      let j = t.t_query.M.cores in
      if j <= 0 then max_cores st else j

  let log_state st =
    Lwt_log.ign_info_f ~section
      "(daemon_state: %d running, %d / %d cores, %d waiting, %d clients)"
      (Full_id_map.cardinal st.active) (used_cores st) (st.max_cores)
      (Q.size st.waiting) (Int_map.cardinal st.clients)

  let make ~forever config_files =
    let config =
      if Sys.file_exists main_config_file
      then Config.parse_or_empty main_config_file else Config.empty
    in
    let config = Config.parse_files config_files config in
    {
      id_count=0;
      msg_count=0;
      max_cores=Config.get_int ~default:1 config "cores";
      accept=true;
      active=Full_id_map.empty;
      clients=Int_map.empty;
      waiting=Q.empty;
      events=Lwt_mvar.create_empty ();
      last_event=Unix.gettimeofday();
      forever;
    }
end

type state = State.t

let maybe_str = function
  | None -> "<none>"
  | Some s -> s

let show_msg_short (msg:M.t): string =
  (M.show msg |> CCString.lines |> List.hd) ^ "…"

let client_send (c:client_conn)(m:M.t): unit Lwt.t =
  Lwt_log.ign_debug_f ~section "(client_send :to %d `%s`)" c.c_id (show_msg_short m);
  M.print c.c_out m

let client_get (c:client_conn): M.t Lwt.t =
  let%lwt msg = M.parse c.c_in in
  Lwt_log.ign_debug_f ~section "(client_get :from %d `%s`)" c.c_id (show_msg_short msg);
  Lwt.return msg

(* reply to client's "status" query *)
let handle_status (st:state) (c:client_conn): unit Lwt.t =
  Lwt_log.ign_info_f ~section "replying to client %d with status" c.c_id;
  let waiting =
    Q.fold
      (fun acc task ->
         let x = {
           M.waiting_id = task.t_query.M.uid; waiting_job = task.t_query;
         } in
         x :: acc)
      [] (State.waiting st)
  and current =
    State.active_l st
    |> List.rev_map
      (fun at ->
         { M.current_job=at.at_task.t_query;
           current_start=at.at_start; })
  in
  let ans = M.StatusAnswer {M.waiting; current; max_cores = State.max_cores st} in
  let%lwt () = client_send c ans in
  Lwt.return_unit

(* handle one client: expect "start" and then just forward
   all incoming messages to scheduler thread *)
let handle_client (st:state) (c:client_conn): unit Lwt.t =
  let rec loop() =
    let%lwt msg = client_get c in
    let%lwt () = State.send_event st (E_recv (c, msg)) in
    loop ()
  in
  let%lwt _ = M.expect c.c_in (M.equal M.Start) in
  loop ()

(* broadcast the message to all clients *)
let broadcast (st:state)(from:client_conn)(msg:M.t): unit Lwt.t =
  State.clients st
  |> Int_map.to_list
  |> Lwt_list.iter_p
    (fun (_,c) ->
       if c.c_id <> from.c_id then client_send c msg else Lwt.return_unit)

(* after this number of seconds without clients, daemon dies *)
let delay_before_dying = 300.

(* in some amount of time, send "refresh" *)
let schedule_refresh (st:state) =
  Lwt.async
    (fun () ->
       let%lwt () = Lwt_unix.sleep (delay_before_dying +. 2.) in
       State.send_event st E_refresh)

let close_chans c: unit Lwt.t =
  try%lwt
    let%lwt () = Lwt_io.close c.c_out in
    let%lwt () = Lwt_io.close c.c_in in
    Lwt.return_unit
  with e ->
    Lwt_log.ign_error_f "error when closing conn to client %d: %s" c.c_id
      (Printexc.to_string e);
    Lwt.return_unit

let kill_task (st:State.t) (at:active_task): unit =
  State.remove_active st at;
  let c = at.at_task.t_conn  in
  c.c_active <- Full_id_map.remove (active_id at) c.c_active;
  Lwt_log.ign_debug_f ~section "task %s: done" (active_id at |> Full_id.to_string);
  ()

(* end of connection for this client *)
let kill_client (st:State.t)(c:client_conn): unit Lwt.t =
  Lwt_log.ign_info_f "stop client %d" c.c_id;
  Lwt.cancel c.c_thread;
  State.remove_client st c;
  close_chans c >>= fun () ->
  (* also kill active tasks of this client *)
  Full_id_map.values c.c_active (kill_task st);
  Lwt.return_unit

(* scheduler: receives requests from several clients, and pings them back *)
let run_scheduler (st:state): unit Lwt.t =
  (* find if a waiting task can be activated, otherwise
     wait for incoming messages *)
  let rec loop () =
    State.log_state st;
    if State.cores_needed st <= State.max_cores st - State.used_cores st then (
      (* try to activate a waiting task *)
      begin match State.take_task st with
        | None ->
          if not (State.forever st) &&
             State.used_cores st = 0 &&
             Int_map.is_empty (State.clients st) &&
             State.num_active st = 0 &&
             State.time_since_last_event st >= delay_before_dying
          then (
            (* only exit if no clients are connected, to avoid the
               race condition:
               - client connects
               - queue is empty --> scheduler stops
               - client sends "acquire" and never gets an answer *)
            Lwt_log.ign_info ~section "no more tasks nor clients, exit";
            Lwt.return_unit
          ) else (
            (* no task, just listen for next event *)
            wait_for_next_event ()
          )
        | Some task ->
          (* start the given process *)
          let uid = task.t_query.M.uid in
          Lwt_log.ign_info_f ~section "start task %s (user %s, pid %d): %s"
            (task_id task |> Full_id.to_string)
            (maybe_str task.t_query.M.user)
            task.t_query.M.pid
            (maybe_str task.t_query.M.info);
          let at = {
            at_task=task;
            at_start=Unix.gettimeofday();
          } in
          (* greenlight the task *)
          State.add_active st at;
          task.t_conn.c_active <-
            Full_id_map.add (active_id at) at task.t_conn.c_active;
          let%lwt () = client_send task.t_conn (M.Go uid) in
          loop ()
      end
    ) else wait_for_next_event ()
  (* listen for new messages. [task] is the current running task, if any *)
  and wait_for_next_event () =
    let%lwt e = State.next_event st in
    begin match e with
      | E_recv (client,msg) ->
        Lwt_log.ign_debug_f ~section
          "(process_message :from %d :msg `%s`)" client.c_id (show_msg_short msg);
        process_incoming_msg client msg
      | E_refresh ->
        Lwt_log.ign_debug ~section "process event 'refresh'";
        loop ()
    end
  (* process one incoming message *)
  and process_incoming_msg (c:client_conn) (msg:M.t) = match msg with
    | M.Status ->
      let%lwt () = handle_status st c in
      loop()
    | M.Start ->
      Lwt_log.ign_error_f ~section
        "invalid duplicate `Start` message for client %d" c.c_id;
      let%lwt () = kill_client st c in
      loop()
    | M.Pong _ ->
      (*Lwt_log.ign_debug_f "got 'pong' from client %d" c.c_id;*)
      c.c_last_pong <- Unix.gettimeofday();
      loop()
    | M.End ->
      Lwt_log.ign_info_f ~section "closed connection to client %d" c.c_id;
      let%lwt () = kill_client st c in
      schedule_refresh st;
      loop ()
    | M.Acquire query ->
      if State.accept st then (
        Lwt_log.ign_info_f ~section "new acquire from client %d (id %d)"
          c.c_id query.M.uid;
        (* add task to scheduler, will be treated as soon as there is room for it *)
        let task = {
          t_conn=c;
          t_query=query;
        } in
        State.push_task st task;
      ) else (
        (* reject task, not accepting any anymore *)
        Lwt.async (fun () -> client_send c (M.Reject query.M.uid))
      );
      loop ()
    | M.Release u ->
      (* find corresponding task and release it *)
      let id = Full_id.make c.c_id u in
      let%lwt() = match State.find_active st id with
        | None ->
          Lwt_log.ign_error_f ~section "client %d released unknown task %d"
            c.c_id u;
          kill_client st c;
        | Some at ->
          Lwt_log.ign_debug_f ~section "client %d released task %d" c.c_id u;
          kill_task st at; (* task is done, we can remove it *)
          Lwt.return_unit
      in
      loop ()
    | M.StopAccepting ->
      Lwt_log.ign_info ~section "stop accepting jobs...";
      State.stop_accept st;
      loop ()
    | (M.Start_bench _ | M.Finish_bench | M.Event _) as msg ->
      (* broadcast, but do not wait for it to terminate *)
      Lwt.async (fun () -> broadcast st c msg);
      loop ()
    | ( M.Ping _ | M.StatusAnswer _ | M.Go _ | M.Reject _) as msg ->
      Lwt_log.ign_error_f "unexpected message: %s" (M.show msg);
      loop ()
  (* delete the task to make room for others *)
  in
  loop ()

let ping_delay = 5.

(* regularly ping clients, and kill these which have not answered to previous
   ping *)
let run_ping_thread st: unit Lwt.t =
  (* n: current "ping" id *)
  let rec loop (n:int) =
    let%lwt () = Lwt_unix.sleep ping_delay in
    (*Lwt_log.ign_debug_f "send ping [%d] to clients" n;*)
    let killed_any = ref false in
    let clients = State.clients st in
    Int_map.iter
      (fun _ c ->
         if c.c_last_ping > c.c_last_pong +. ping_delay +. 2. 
         then (
           killed_any := true;
           Lwt.async (fun () -> kill_client st c) (* dead *)
         ) else (
           c.c_last_ping <- Unix.gettimeofday();
           Lwt.async (fun () ->
             try%lwt client_send c (M.Ping n)
             with _ -> Lwt.return_unit)
         ))
      clients;
    (* might have to refresh state *)
    if !killed_any then (
      Lwt.async (fun () -> State.send_event st E_refresh)
    );
    loop (n+1)
  in
  loop 0

(* spawn a daemon, to listen on the given port *)
let spawn ?(forever=false) (port:int): unit Lwt.t =
  Lwt_log.ign_info ~section "---------------------------";
  Lwt_log.ign_info_f ~section "starting daemon on port %d" port;
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let st = State.make ~forever [] in
  (* scheduler *)
  Lwt_log.ign_info ~section "start scheduler";
  let scheduler_thread = run_scheduler st in
  Lwt_log.ign_info ~section "scheduler started";
  (* ping clients regularly *)
  let ping_thread = run_ping_thread st in
  (* server that listens for incoming clients *)
  let server = Lwt_io.establish_server addr
    (fun (ic,oc) ->
      let c = {
        c_in=ic;
        c_out=oc;
        c_last_ping=Unix.gettimeofday();
        c_last_pong=Unix.gettimeofday();
        c_id=State.new_id st;
        c_thread=Lwt.return_unit;
        c_active=Full_id_map.empty;
      } in
      Lwt.async
        (fun () ->
           try%lwt
             let th = handle_client st c in
             State.add_client st c;
             c.c_thread <- th;
             th >>= fun () -> close_chans c
           with _ -> close_chans c))
  in
  (* stop *)
  Lwt_log.ign_debug ~section "daemon started";
  let%lwt () =
    Lwt.pick
      [ scheduler_thread;
        ping_thread;
      ] in
  Lwt_log.ign_debug ~section "daemon's server is stopping";
  Lwt_io.shutdown_server server;
  Lwt.return_unit

(* TODO: change log level through connection *)

let setup_loggers file_name () =
  let syslog = Lwt_log.syslog ~facility:`User () in
  Lwt_log.default := syslog;
  Lwt_log.add_rule "*" Lwt_log.Debug;
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

(* init function: check if we are within a daemon call (i.e. the main process
   that has called itself with DAEMON_PORT=<port> to start a daemon),
   in which case we call {!spawn} and then exit *)
let () = match Sys.getenv "DAEMON_PORT" |> int_of_string with
  | p ->
    (* Printf.printf "run as daemon on port %d\n%!" p; *)
    Lwt_daemon.daemonize ~syslog:true ~directory:"/tmp"
      ~stdin:`Close ~stdout:`Close ~stderr:`Keep ();
    let log_file =
      let config = Config.parse_or_empty main_config_file in
      Config.get_string ~default:"/tmp/froglock.log" config "log"
    in
    Lwt_main.run (
      let%lwt () = setup_loggers log_file () in
      spawn p
    );
    exit 0
  | exception _ -> ()

let fork_daemon port : unit =
  Lwt.async
    (fun () ->
       let cmd = Sys.executable_name, [| Sys.executable_name |] in
       let env = [| "DAEMON_PORT=" ^ string_of_int port |] in
       Lwt_process.exec ~env cmd);
  ()

