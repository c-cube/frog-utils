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


(* read line from [ic], filter it with f *)
let expect ic f =
  Lwt_io.read_line ic >>= fun line ->
  match f line with
  | None ->
      Lwt_log.ign_debug_f "got unexpected %s" line;
      Lwt.fail (InvalidMessage line)
  | Some x -> Lwt.return x

let expect_str ic line =
  expect ic (fun l -> if l=line then Some () else None)

let write_line oc line = Lwt_io.write_line oc line

(** {2 Daemon Code} *)

(* TODO: a second server, on a second port, for monitoring
   TODO: change log level through connection *)

module Daemon = struct
  type task = {
    box : unit Lwt_mvar.t;
    id : int;
  }

  (* scheduler: receives requests from several clients, and pings them back *)
  let start_scheduler () =
    let q = Queue.create () in
    let inbox = Lwt_mvar.create_empty () in
    (* listen for new messages. [task] is the current running task, if any *)
    let rec listen cur_task =
      Lwt_mvar.take inbox >>= function
      | `Register task' ->
          Queue.push task' q;
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
      if Queue.is_empty q
      then listen None
      else (
        (* start the given process *)
        let task = Queue.take q in
        Lwt_log.ign_info_f "start task %d" task.id;
        Lwt_mvar.put task.box () >>= fun () ->
        listen (Some task)
      )
    in
    let thread = listen None in
    inbox, thread

  (* handle one client.
    [cond_stop] condition to stop the server
    [ic,oc] connection to client *)
  let handle_client scheduler_inbox id (ic, oc) =
    Lwt_log.ign_debug_f "task %d: wait for acquire..." id;
    expect_str ic "acquire" >>= fun _ ->
    let task = {box=Lwt_mvar.create_empty (); id} in
    (* acquire lock *)
    Lwt_mvar.put scheduler_inbox (`Register task) >>= fun () ->
    Lwt_mvar.take task.box >>= fun () ->
    let release_ () =
      (* release lock *)
      Lwt_log.ign_debug_f "task %d: released" id;
      Lwt_mvar.put scheduler_inbox (`Done task)
    in
    Lwt.catch
      (fun () ->
        (* start task *)
        Lwt_log.ign_debug_f "task %d: send 'go'" id;
        write_line oc "go" >>= fun () ->
        expect_str ic "release" >>= fun () ->
        release_ ()
      ) (fun _ -> release_ ())

  (* spawn a daemon, to listen on the given port *)
  let spawn port =
    Lwt_log.ign_info_f "starting daemon on port %d" port;
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    let id = ref 0 in
    (* scheduler *)
    Lwt_log.ign_info "start scheduler";
    let scheduler, run_scheduler = start_scheduler () in
    Lwt_log.ign_info "scheduler started";
    (* server that listens for incoming clients *)
    let server = Lwt_io.establish_server addr
      (fun (ic,oc) ->
        let i = !id in
        incr id;
        Lwt_log.ign_info_f "received new query (id %d)" i;
        Lwt.async (fun () -> handle_client scheduler i (ic,oc))
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
  let acquire ic oc f =
    Lwt_log.ign_debug "acquiring lock...";
    write_line oc "acquire" >>= fun () ->
    expect_str ic "go" >>= fun () ->
    Lwt_log.ign_debug "acquired lock";
    Lwt.finalize
      f
      (fun () ->
        Lwt_log.ign_debug "release lock";
        write_line oc "release"
      )

  (* connect to the given port. *)
  let connect_and_acquire port f =
    Lwt_log.ign_debug_f "trying to connect to daemon on port %d..." port;
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    Lwt_io.with_connection addr
      (fun (ic,oc) ->
        Lwt_log.ign_debug_f "connected to daemon";
        acquire ic oc f
      )

  (* try to connect; if it fails, spawn daemon and retry *)
  let acquire_or_spawn port f =
    Lwt.catch
      (fun () -> connect_and_acquire port f)
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
            connect_and_acquire port f
      )
end

(* result of running a command *)
type result = {
  out : string;
  time : float;  (* running time *)
  status : Unix.process_status;
  pid : int;
}

(* main task: acquire lock file, execute command [cmd], release lock *)
let run_command port prog args =
  Client.acquire_or_spawn port
    (fun () ->
      let cmd = prog, Array.of_list (prog::args) in
      Lwt_log.ign_debug_f "start command %s" (String.concat " " (prog::args));
      let start = Unix.gettimeofday () in
      Lwt_process.with_process cmd
        (fun process ->
          (* launch command, read its output *)
          Lwt_io.read process#stdout >>= fun out ->
          process#status >>= fun status ->
          (* measure time elapsed since we started the process *)
          let stop = Unix.gettimeofday () in
          let time = stop -. start in
          Lwt_log.ign_debug_f "command finished after %.2fs" time;
          let res = {out; time; status; pid=process#pid; } in
          Lwt.return res
        )
    )

let main ?(debug=false) ~port prog args =
  Lwt_main.run
    (
      Lwt_log.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ();
      if debug then (
        Lwt_log.add_rule "*" Lwt_log.Debug
      );
      run_command port prog args >>= fun res ->
      (* TODO: print more details, like return code *)
      Lwt_log.ign_info_f "# process ran in %.2fs (pid: %d)\n" res.time res.pid;
      Lwt_io.print res.out
    )

(** {2 Main} *)

let port_ = ref 12000
let cmd_ = ref []
let debug_ = ref false
let push_cmd_ s = cmd_ := s :: !cmd_

let options =
  [ "-port", Arg.Set_int port_, "local port for the daemon"
  ; "-debug", Arg.Set debug_, "enable debug"
  ]
(* TODO: option to send a mail when the job finishes *)
(* TODO: option to specify estimated completion time *)

let () =
  Arg.parse options push_cmd_ "locke [options] <cmd> <args>";
  let cmd = List.rev !cmd_ in
  match cmd with
  | [] -> print_endline "no command"
  | head::args ->
    main ~debug:!debug_ ~port:!port_ head args
