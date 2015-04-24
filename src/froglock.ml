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

let section = Lwt_log.Section.make "FrogLock"

let () =
  FrogLockMessages.register_exn_printers();
  Lwt.async_exception_hook :=
    (fun e ->
      Lwt_log.ign_error_f "async error: %s" (Printexc.to_string e);
      exit 1)

let write_line oc line = Lwt_io.write_line oc line

type cmd =
  | Shell of string
  | Exec of string * string list
  | PrintStatus
  | StopAccepting
  [@@deriving show]

type parameters = {
  port : int;
  cmd : cmd;
  debug : bool;
  priority : int;
  cores : int;
  tags : string list; (* user-defined tags *)
}

(* result of running a command *)
type result = {
  res_cmd : string;
  time : float;  (* running time *)
  status : Unix.process_status;
  pid : int;
}

(* main task: acquire lock file, execute command [cmd], release lock *)
let run_command params =
  let info = show_cmd params.cmd in
  let user = try Some(Sys.getenv "USER") with _ -> None in
  let cwd = Sys.getcwd () in
  FrogLockClient.connect_or_spawn ~log_file:"/tmp/froglock.log" params.port
    (fun daemon ->
      FrogLockClient.acquire ~cwd ?user ~info ~cores:params.cores ~priority:params.priority ~tags:params.tags daemon
        (function
        | true ->
          let cmd, cmd_string = match params.cmd with
            | PrintStatus
            | StopAccepting -> assert false
            | Shell c -> Lwt_process.shell c, c
            | Exec (prog, args) ->
                let cmd = prog, Array.of_list (prog::args) in
                cmd, (String.concat " " (prog::args))
          in
          Lwt_log.ign_debug_f "start command %s" cmd_string;
          let start = Unix.gettimeofday () in
          (* close stdin so that interactive commands fail *)
          Lwt_process.with_process_none ~stdin:`Close ~stdout:`Keep cmd
            (fun process ->
              let%lwt status = process#status in
              (* measure time elapsed since we started the process *)
              let stop = Unix.gettimeofday () in
              let time = stop -. start in
              Lwt_log.ign_debug_f ~section "command finished after %.2fs" time;
              let res = {res_cmd=cmd_string; time; status; pid=process#pid; } in
              Lwt.return (Some res)
            )
      | false ->
          Lwt_log.ign_info_f "could not acquire lock";
          Lwt.return_none
      )
    )

module M = FrogLockMessages

let maybe_str = function
  | None -> "<none>"
  | Some s -> s

(* connect to daemon (if any) and ask status *)
let print_status params =
  let tags2str tags = match tags with
    | [] -> ""
    | [t] -> ", tag " ^ t
    | l -> ", tags {" ^ String.concat ", " l ^ "}"
  in
  let now = Unix.gettimeofday() in
  try%lwt
    let%lwt res = FrogLockClient.get_status params.port in
    match res with
    | None ->
      Lwt_log.info_f ~section "daemon not running"
    | Some {M.current=l; waiting} ->
      let%lwt () = match l with
        | [] -> Lwt.return_unit
        | _ ->
          Lwt_list.iter_s (fun c ->
              let time = Unix.gettimeofday() -. c.M.current_start in
              let job = c.M.current_job in
              Lwt_io.printlf
                "current job (cores %d, user %s, pid %d, cwd %s, issued %.2fs ago%s, running for %.2fs): %s"
                job.M.cores
                (maybe_str job.M.user)
                job.M.pid (maybe_str job.M.cwd)
                (now -. job.M.query_time)
                (tags2str job.M.tags)
                time (maybe_str job.M.info)
            ) l
      in
      Lwt_list.iter_s
        (fun job ->
           Lwt_io.printlf "waiting job n°%d (cores %d, user %s, pid %d, cwd %s, issued %.2fs ago%s): %s"
             (job.M.id :> int)
             job.M.cores
             (maybe_str job.M.user)
             job.M.pid (maybe_str job.M.cwd)
             (now -. job.M.query_time)
             (tags2str job.M.tags)
             (maybe_str job.M.info)
        ) waiting
  with e ->
     Lwt_log.ign_error_f ~section "error: %s" (Printexc.to_string e);
     Lwt.return_unit

let main params =
  Lwt_main.run
    (
      Lwt_log.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ();
      if params.debug
        then Lwt_log.add_rule "*" Lwt_log.Debug;
      match params.cmd with
      | PrintStatus -> print_status params
      | StopAccepting -> FrogLockClient.stop_accepting params.port
      | Exec _
      | Shell _ ->
          let%lwt res = run_command params in
          match res with
          | None -> Lwt.return_unit
          | Some res ->
            (* TODO: print more details, like return code *)
            Lwt_log.ign_info_f ~section "process ran in %.2fs (pid: %d)\n"
              res.time res.pid;
            Lwt.return_unit
    )

(** {2 Main} *)

(* TODO: option to specify estimated completion time *)
(* TODO: dynamic plugins, that can add their own options to [options] *)

let common_opts =
  let open Cmdliner in
  let aux port debug tags priority cores cmd = { port; debug; tags; priority; cores; cmd }  in
  let port =
    let doc = "Local port for the daemon" in
    Arg.(value & opt int 12000 & info ["p"; "port"] ~docv:"PORT" ~doc)
  in
  let debug =
    let doc = "Enable debug" in
    Arg.(value & flag & info ["d"; "debug"] ~doc)
  in
  let tags =
    let doc = "Add a user-defined tag to a job" in
    Arg.(value & opt_all string [] & info ["t"; "tag"] ~docv:"TAG" ~doc)
  in
  let prio =
    let doc = "Priority of the job. Higher priority jobs will be run before
               lower priority jobs." in
    Arg.(value & opt int 10 & info ["prio"] ~docv:"PRIO" ~doc)
  in
  let cores =
    let doc = "Number of cores to lock fro this task (default= all)." in
    Arg.(value & opt int 0 & info["j"; "cores"] ~docv:"CORES" ~doc)
  in
  Term.(pure aux $ port $ debug $ tags $ prio $ cores)

(*
let shell_term =
  let open Cmdliner in
  let aux c = Shell c in
  let cmd =
    let doc = "The command to be executed" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"CMD" ~doc)
  in
  let doc = "Execute a shell command after acquiring a global lock" in
  let man = [
    `S "DESCRIPTION";
    `P "Executes the given shell command after acquiring a global lock.
        If another command already has the lock, wait for it to be released.";
  ] in
  Term.(pure main $ (common_opts $ (pure aux $ cmd))),
  Term.info ~doc ~man "shell"
*)

let status_term =
  let open Cmdliner in
  let doc = "Print the status of the deamon, then exit." in
  let man = [
    `S "DESCRIPTION";
    `P "Asks the daemon to prints its status, including
        the commands waiting to run.
        and statistics about the command currently running."
  ] in
  Term.(pure main $ (common_opts $ pure PrintStatus)),
  Term.info ~man ~doc "status"

let stop_term =
  let open Cmdliner in
  let doc = "Tell the daemon to stop accepting new jobs" in
  let man = [
    `S "DESCRIPTION";
    `P "Send a message to the daemon, telling it to stop accepting new jobs.
        Jobs already waiting to be run are not affected.";
  ] in
  Term.(pure main $ (common_opts $ pure StopAccepting)),
  Term.info ~man ~doc "stop"

let term =
  let open Cmdliner in
  let aux shell cmds =
    if shell then
      Shell (String.concat " " cmds)
    else match cmds with
      | cmd :: args -> Exec (cmd, args)
      | _ -> assert false
  in
  let shell =
    let doc = "Invoke command in a shell" in
    Arg.(value & flag & info ["c"; "shell"] ~doc)
  in
  let cmd =
    let doc = "Command to execute" in
    Arg.(non_empty & pos_all string [] & info [] ~docv:"CMD" ~doc)
  in
  let doc = "Execute commands after acquiring a global lock." in
  let man = [
    `S "SYNOPSIS";
    `I ("$(b,froglock COMMAND)", "Use a command");
    `I ("$(b,froglock [OPTIONS] -- CMD [CMD [...]])", "Run the given command after acquiring a global lock.");
    `S "DESCRIPTION";
    `P "This tool uses a daemon to enforce a global lock on commands, so that no two commands
        executed through this tool will run at the same time. The daemon listens on a specific
        port, which can be specified in the options. If no daemon listens on the given port,
        one will be automatically launched.";
  ] in
  Term.(pure main $ (common_opts $ (pure aux $ shell $ cmd))),
  Term.info ~man ~doc "froglock"

let () =
  match Cmdliner.Term.eval_choice term [status_term; stop_term] with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok () -> ()

