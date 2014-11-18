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

(** {1 Scheduling script} *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

exception InvalidMessage of string

let () =
  Printexc.register_printer
    (function
      | (InvalidMessage s) -> Some ("invalid message: " ^ s)
      | _ -> None);
  Lwt.async_exception_hook :=
    (fun e ->
      Lwt_log.ign_error_f "async error: %s" (Printexc.to_string e);
      exit 1)

let write_line oc line = Lwt_io.write_line oc line
module Message = struct
  (* a message "acquire" *)
  type acquire_query = {
    info : string;
    pid : int;
  } [@@deriving yojson,show]

  type waiting_job = {
    waiting_pid : int;
    waiting_info : string;
  } [@@deriving yojson,show]

  type status_answer = {
    waiting : waiting_job list;
  } [@@deriving yojson,show]

  type t =
    | Acquire of acquire_query
    | Release
    | Go (* acquisition succeeded *)
    | Status
    | StatusAnswer of status_answer
    [@@deriving yojson, show]

  let is_go = function Go -> true | _ -> false
  let is_release = function Release -> true | _ -> false

  exception Unexpected of t

  let () =
    Printexc.register_printer
     (function
      | Unexpected t -> Some ("unexpected message " ^ show t)
      | _ -> None)

  let expect ic p =
    Lwt_io.read_line ic >>= fun s ->
    Lwt.wrap (fun () -> Yojson.Safe.from_string s)
    >|= of_yojson
    >>= function
    | `Ok m when p m -> Lwt.return m
    | `Ok _ -> Lwt.fail (InvalidMessage ("unexpected " ^ s))
    | `Error msg -> Lwt.fail (InvalidMessage (msg ^ ": " ^ s))

  let parse ic = expect ic (fun _ -> true)

  let print oc m =
    let s = Yojson.Safe.to_string (to_yojson m) in
    Lwt_io.write_line oc s
end

(** {2 Daemon Code} *)

(* TODO: change log level through connection *)

module Daemon = struct
  type acquire_task = {
    box : unit Lwt_mvar.t;
    id : int;
    query : Message.acquire_query;
  }

  type msg =
    [ `Register of acquire_task
    | `Done of acquire_task
    ]

  type state = {
    mutable num_clients : int;
    mutable cur_id : int;
    queue : acquire_task Queue.t;
    scheduler : msg Lwt_mvar.t;
  }

  (* scheduler: receives requests from several clients, and pings them back *)
  let start_scheduler ~state () =
    let inbox = state.scheduler in
    (* listen for new messages. [task] is the current running task, if any *)
    let rec listen cur_task =
      Lwt_mvar.take inbox >>= function
      | `Register task' ->
          Queue.push task' state.queue;
          if cur_task=None
            then run_next ()
            else listen cur_task
      | `Done task' ->
          begin match cur_task with
          | Some t when t.id = task'.id ->
              (* task if finished, run the next one *)
              Lwt_log.ign_info_f "task %d finished" t.id;
              run_next ()
          | _ ->
            Lwt_log.ign_error_f "scheduler: unexpected 'Done' for task %d" task'.id;
            listen cur_task
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
          Lwt_log.ign_info "no more tasks nor clients, exit";
          Lwt.return_unit
        ) else listen None
      else (
        (* start the given process *)
        let task = Queue.take state.queue in
        Lwt_log.ign_info_f "start task %d (pid %d): %s"
          task.id task.query.Message.pid task.query.Message.info;
        Lwt_mvar.put task.box () >>= fun () ->
        listen (Some task)
      )
    in
    listen None

  let handle_acquire ~state id (ic,oc) query =
    let task = {box=Lwt_mvar.create_empty (); id; query} in
    (* acquire lock *)
    Lwt_mvar.put state.scheduler (`Register task) >>= fun () ->
    Lwt_mvar.take task.box >>= fun () ->
    let release_ () =
      (* release lock *)
      Lwt_log.ign_debug_f "task %d: released" id;
      state.num_clients <- state.num_clients - 1;
      Lwt_mvar.put state.scheduler (`Done task)
    in
    Lwt.catch
      (fun () ->
        (* start task *)
        Lwt_log.ign_debug_f "task %d: send 'go'" id;
        Message.print oc Message.Go >>= fun () ->
        Message.expect ic Message.is_release >>= fun _ ->
        release_ ()
      ) (fun _ -> release_ ())

  let handle_status ~state oc =
    let module M = Message in
    let jobs = Queue.fold
      (fun acc job ->
        {M.waiting_pid=job.query.M.pid; waiting_info=job.query.M.info} :: acc
      ) [] state.queue
    in
    let jobs = List.rev jobs in
    let ans = M.StatusAnswer {M.waiting=jobs} in
    M.print oc ans

  (* handle one client.
    [cond_stop] condition to stop the server
    [ic,oc] connection to client *)
  let handle_client ~state id (ic, oc) =
    state.num_clients <- state.num_clients + 1;
    Lwt_log.ign_debug_f "task %d: wait for acquire..." id;
    Message.parse ic >>= function
    | Message.Acquire q ->
        handle_acquire ~state id (ic,oc) q
    | Message.Status ->
        handle_status ~state oc
    | msg ->
        Lwt.fail (Message.Unexpected msg)

  (* spawn a daemon, to listen on the given port *)
  let spawn port =
    Lwt_log.ign_info_f "starting daemon on port %d" port;
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    let scheduler = Lwt_mvar.create_empty () in
    let state = {num_clients=0; cur_id=0; scheduler; queue=Queue.create(); } in
    (* scheduler *)
    Lwt_log.ign_info "start scheduler";
    let run_scheduler = start_scheduler ~state () in
    Lwt_log.ign_info "scheduler started";
    (* server that listens for incoming clients *)
    let server = Lwt_io.establish_server addr
      (fun (ic,oc) ->
        let id = state.cur_id in
        state.cur_id <- state.cur_id + 1;
        Lwt_log.ign_info_f "received new query (id %d)" id;
        Lwt.async (fun () -> handle_client ~state id (ic,oc))
      )
    in
    (* stop *)
    Lwt_log.ign_debug "daemon started";
    run_scheduler >>= fun () ->
    Lwt_log.ign_debug "daemon's server is stopping";
    Lwt_io.shutdown_server server;
    Lwt.return_unit

  (* fork and spawn a daemon on the given port *)
  let fork_and_spawn port =
    match Lwt_unix.fork () with
    | 0 -> (* child, will be the daemon *)
      Lwt_daemon.daemonize ~syslog:false ~directory:"/tmp"
        ~stdin:`Close ~stdout:`Close ~stderr:`Close ();
      (* change logger *)
      Lwt_log.file ~mode:`Append ~file_name:"/tmp/join_lock.log" ()
      >>= fun logger ->
      Lwt_log.default := logger;
      Lwt_log.add_rule "*" Lwt_log.Info;
      Lwt.return (`child (spawn port))
    | _ -> Lwt.return `parent
end

(** {2 Client Side} *)

module Client = struct
  (* given the channels to the daemon, acquire lock, call [f], release lock *)
  let acquire ic oc ~info f =
    Lwt_log.ign_debug "acquiring lock...";
    (* send "acquire" *)
    let pid = Unix.getpid() in
    let msg = Message.(Acquire {info; pid}) in
    Message.print oc msg >>= fun () ->
    (* expect "go" *)
    Message.expect ic Message.is_go >>= fun _ ->
    Lwt_log.ign_debug "acquired lock";
    Lwt.finalize
      f
      (fun () ->
        (* eventually, release *)
        Lwt_log.ign_debug "release lock";
        write_line oc "release"
      )

  let connect port f =
    Lwt_log.ign_debug_f "trying to connect to daemon on port %d..." port;
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    Lwt_io.with_connection addr
      (fun (ic,oc) ->
        Lwt_log.ign_debug_f "connected to daemon";
        f ic oc
      )

  (* connect to the given port. *)
  let connect_and_acquire port ~info f =
    connect port
      (fun ic oc ->
        acquire ic oc ~info f
      )

  (* try to connect; if it fails, spawn daemon and retry *)
  let acquire_or_spawn port ~info f =
    Lwt.catch
      (fun () -> connect_and_acquire port ~info f)
      (fun _e ->
        (* launch daemon and re-connect *)
        Lwt_log.ign_info "could not connect; launch daemon...";
        Lwt_io.flush_all() >>= fun () ->
        Daemon.fork_and_spawn port >>= function
        | `child thread ->
            thread >>= fun() ->
            Lwt.fail Exit
        | `parent ->
            Lwt_unix.sleep 1. >>= fun () ->
            Lwt_log.ign_info "retry to connect to daemon...";
            connect_and_acquire port ~info f
      )
end

type cmd =
  | Shell of string
  | Exec of string * string list
  | PrintStatus
  [@@deriving show]

type parameters = {
  mails : string list;
  port : int;
  cmd : cmd;
  debug : bool;
}

(* result of running a command *)
type result = {
  res_cmd : string;
  out : string;
  time : float;  (* running time *)
  status : Unix.process_status;
  pid : int;
}

(* main task: acquire lock file, execute command [cmd], release lock *)
let run_command params =
  let info = show_cmd params.cmd in
  Client.acquire_or_spawn params.port ~info
    (fun () ->
      let cmd, cmd_string = match params.cmd with
        | PrintStatus -> assert false
        | Shell c -> Lwt_process.shell c, c
        | Exec (prog, args) ->
            let cmd = prog, Array.of_list (prog::args) in
            cmd, (String.concat " " (prog::args))
      in
      Lwt_log.ign_debug_f "start command %s" cmd_string;
      let start = Unix.gettimeofday () in
      Lwt_process.with_process cmd
        (fun process ->
          (* launch command, read its output line by line and
            display it at the same time *)
          let lines_stream = Lwt_io.read_lines process#stdout in
          let buf = Buffer.create 256 in
          Lwt_stream.iter_s
            (fun line ->
              Buffer.add_string buf line;
              Buffer.add_char buf '\n';
              Lwt_io.printl line
            ) lines_stream
          >>= fun () ->
          process#status >>= fun status ->
          (* measure time elapsed since we started the process *)
          let stop = Unix.gettimeofday () in
          let time = stop -. start in
          Lwt_log.ign_debug_f "command finished after %.2fs" time;
          let res = {res_cmd=cmd_string; out=Buffer.contents buf;
                     time; status; pid=process#pid; } in
          Lwt.return res
        )
    )

(* send a recap mail to the given address *)
let send_mail addr res =
  let real_addr = Smtp_lwt.Addr.of_string addr in
  let i = String.index addr '@' in
  let domain = String.sub addr (i+1) (String.length addr-i-1) in
  let from = "\"join-locke\" <joinlocke@example.com>" in
  Lwt_log.ign_debug_f "try to send a mail to %s..." addr;
  (* connect to SMTP server *)
  Smtp_lwt.connect ~host:domain ~name:"<joinlocke@example.com>" () >>= fun c ->
  let body = Printf.sprintf
    "Subject: job '%s' (%.2fs)\n\
    From: %s\n\
    To: %s \n\
    \n\
    %s" (String.escaped res.res_cmd) res.time from addr res.out in
  Smtp_lwt.send c
    ~from:(Smtp_lwt.Addr.of_string from)
    ~to_:[real_addr]
    ~body
  >>= function
  | `Ok (_,_) ->
      Lwt_log.ign_debug_f "succeeded in sending the mail to %s" addr;
      Lwt.return_unit
  | `Failure (c,msg) ->
      let msg = Printf.sprintf "could not send mail to %s: %s (code %d)" addr msg c in
      failwith msg

(* send mail to addresses *)
let send_mails params res =
  Lwt_list.iter_p
    (fun addr ->
      Lwt.catch
        (fun () -> send_mail addr res)
        (fun e ->
          let msg = Printexc.to_string e in
          Lwt_log.error_f "could not send mail to %s: %s" addr msg)
    ) params.mails

(* connect to daemon (if any) and ask status) *)
let print_status params =
  let module M = Message in
  Lwt.catch
    (fun () ->
      Client.connect params.port
        (fun ic oc ->
          M.print oc M.Status >>= fun () ->
          M.parse ic >>= function
          | M.StatusAnswer l ->
              Lwt_list.iter_s
                (fun job ->
                  Lwt_io.printlf "waiting job (pid %d): %s"
                    job.M.waiting_pid job.M.waiting_info
                ) l.M.waiting
          | m ->
              Lwt.fail (M.Unexpected m)
        )
    )
    (fun e ->
      Lwt_log.ign_error_f "error: %s" (Printexc.to_string e);
      Lwt.return_unit
    )

let main params =
  Lwt_main.run
    (
      Lwt_log.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ();
      if params.debug then (
        Lwt_log.add_rule "*" Lwt_log.Debug
      );
      match params.cmd with
      | PrintStatus ->
          print_status params
      | Exec _
      | Shell _ ->
          run_command params >>= fun res ->
          (* TODO: print more details, like return code *)
          Lwt_log.ign_info_f "process ran in %.2fs (pid: %d)\n" res.time res.pid;
          send_mails params res
    )

(** {2 Main} *)

let port_ = ref 12000
let cmd_ = ref []
let debug_ = ref false
let shell_ = ref None
let mails_ = ref []
let status_ = ref false

let push_cmd_ s = cmd_ := s :: !cmd_
let set_shell_ s = shell_ := Some s
let add_mail_ s = mails_ := s :: !mails_

let usage = "locke [options] <cmd> <args>"
let options = Arg.align
  [ "-port", Arg.Set_int port_, " local port for the daemon"
  ; "-debug", Arg.Set debug_, " enable debug"
  ; "-mail", Arg.String add_mail_, " add mail address"
  ; "-c", Arg.String set_shell_, " use a shell command"
  ; "-status", Arg.Set status_, " report status of the daemon (if any)"
  ; "--", Arg.Rest push_cmd_, "start parsing command"
  ]

let usage_ = "locke [options] <cmd> <args>"

(* TODO: option to specify estimated completion time *)

let () =
  Arg.parse options push_cmd_ usage_;
  let params = match !shell_, List.rev !cmd_ with
    | _ when !status_ ->
        { debug= !debug_; port= !port_; mails= !mails_; cmd=PrintStatus; }
    | None, [] ->
        Arg.usage options usage_;
        exit 0
    | Some c, _ ->
        { debug= !debug_; port= !port_; mails= !mails_; cmd=Shell c }
    | None, head::args ->
        { debug= !debug_; port= !port_; mails= !mails_; cmd=Exec (head,args) }
  in
  main params
