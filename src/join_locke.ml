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
      | _ -> None)

(* read line from [ic], filter it with f *)
let expect ic f =
  Lwt_io.read_line ic >>= fun line ->
  match f line with
  | None -> Lwt.fail (InvalidMessage line)
  | Some x -> Lwt.return x

let expect_str ic line =
  expect ic (fun l -> if l=line then Some () else None)

(** {2 Daemon Code} *)

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
            Lwt_log.ign_info_f "scheduler: unexpected 'Done' for task %d" task'.id;
            listen cur_task
          end
    (* run task *)
    and run_next () =
      if Queue.is_empty q
      then listen None
      else (
        (* start the given process *)
        let task = Queue.take q in
        Lwt_log.ign_debug_f "start task %d" task.id;
        Lwt_mvar.put task.box () >>= fun () ->
        listen (Some task)
      )
    in
    Lwt.async (fun () -> listen None);
    inbox

  (* handle one client.
    [cond_stop] condition to stop the server
    [ic,oc] connection to client *)
  let handle_client scheduler_inbox id (ic, oc) =
    expect_str ic "acquire" >>= fun _ ->
    let task = {box=Lwt_mvar.create_empty (); id} in
    (* acquire lock *)
    Lwt_mvar.put scheduler_inbox (`Register task) >>= fun () ->
    Lwt_mvar.take task.box >>= fun () ->
    (* start task *)
    Lwt_io.write_line oc "go" >>= fun () ->
    expect_str ic "release" >>= fun () ->
    (* release lock *)
    Lwt_mvar.put scheduler_inbox (`Done task)

  (* TODO: a command to stop the server? *)

  (* spawn a daemon, to listen on the given port *)
  let spawn port =
    Lwt_log.ign_info_f "starting daemon on port %d" port;
    let addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
    let cond = Lwt_condition.create () in
    let id = ref 0 in
    (* scheduler *)
    let scheduler = start_scheduler () in
    (* server that listens for incoming clients *)
    let server = Lwt_io.establish_server addr
      (fun (ic,oc) ->
        let i = !id in
        incr id;
        Lwt.async (fun () -> handle_client scheduler i (ic,oc))
      )
    in
    (* stop *)
    Lwt_condition.wait cond >>= fun () ->
    Lwt_io.shutdown_server server;
    Lwt.return_unit

  (* fork and spawn a daemon on the given port *)
  let fork_and_spawn port =
    Lwt_daemon.daemonize ~stdin:`Close ~stdout:`Close ~stderr:`Close ();
    Lwt_main.run (spawn port)
end

(** {2 Client Side} *)

module Client = struct
  (* given the channels to the daemon, acquire lock, call [f], release lock *)
  let acquire ic oc f =
    Lwt_io.write_line oc "acquire" >>= fun () ->
    expect_str ic "go" >>= fun () ->
    Lwt.finalize
      f
      (fun () -> Lwt_io.write_line oc "release")

  (* connect to the given port. *)
  let connect_daemon port f =
    Lwt_log.ign_debug_f "trying to connect to daemon on port %d..." port;
    let addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
    Lwt_io.with_connection addr
      (fun (ic,oc) ->
        acquire ic oc f
      )

  (* try to connect; if it fails, spawn daemon and retry *)
  let connect_or_spawn port f =
    Lwt.catch
      (fun () -> connect_daemon port f)
      (fun _e ->
        (* launch daemon and re-connect *)
        Lwt_log.ign_info "could not connect; launch daemon...";
        Daemon.fork_and_spawn port;
        Lwt_unix.sleep 3. >>= fun () ->
        Lwt_log.ign_info "retry to connect to daemon...";
        connect_daemon port f
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
  Client.connect_or_spawn port
    (fun () ->
      let cmd = prog, Array.of_list (prog::args) in
      Lwt_process.with_process cmd
        (fun process ->
          (* launch command, read its output *)
          let start = Unix.gettimeofday () in
          Lwt_io.read process#stdout >>= fun out ->
          process#status >>= fun status ->
          (* measure time elapsed since we started the process *)
          let stop = Unix.gettimeofday () in
          let time = stop -. start in
          let res = {out; time; status; pid=process#pid; } in
          Lwt.return res
        )
    )

let main port prog args =
  Lwt_main.run
    ( Lwt_io.printl "start..." >>= fun () ->
      run_command port prog args >>= fun res ->
      (* TODO: print more details, like return code *)
      Lwt_io.printf "# process ran in %.2fs (pid: %d)\n" res.time res.pid >>= fun () ->
      Lwt_io.printl res.out
    )

(** {2 Main} *)

let port_ = ref 8989
let cmd_ = ref []
let push_cmd_ s = cmd_ := s :: !cmd_

let options =
  [ "-port", Arg.Set_int port_, "local port for the daemon"
  ]
(* TODO: option to send a mail when the job finishes *)
(* TODO: option to specify estimated completion time *)

let () =
  Arg.parse options push_cmd_ "locke [options] <cmd> <args>";
  let cmd = List.rev !cmd_ in
  match cmd with
  | [] -> print_endline "no command"
  | head::args ->
    main !port_ head args
