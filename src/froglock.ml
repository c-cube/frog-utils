
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Scheduling script} *)

let section = Logs.Src.create "FrogLock"

let () =
  FrogLockMessages.register_exn_printers();
  ()

let write_line oc line =
  output_string oc line;
  output_char oc '\n';
  flush oc

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
            | Shell c -> Printf.sprintf "/bin/sh -c %s" c, c
            | Exec (prog, args) ->
                let args = String.concat " " args in
                let cmd = Printf.sprintf "%s %s" prog args in
                cmd, args
          in
          Logs.debug ~src:section (fun k->k "start command %s" cmd_string);
          let start = Unix.gettimeofday () in
          (* close stdin so that interactive commands fail *)
          CCUnix.with_process_full cmd
            ~f:(fun p ->
              let status = p#close in
              (* measure time elapsed since we started the process *)
              let stop = Unix.gettimeofday () in
              let time = stop -. start in
              Logs.debug ~src:section (fun k->k "command finished after %.2fs" time);
              (* FIXME: obtain PID!! *)
              let res = {res_cmd=cmd_string; time; status; pid=0; } in
              Some res)
      | false ->
          Logs.info ~src:section (fun k->k "could not acquire lock");
          None)
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
  try
    let res = FrogLockClient.get_status params.port in
    match res with
    | None ->
      Logs.info ~src:section (fun k->k "daemon not running")
    | Some {M.current=l; waiting; max_cores} ->
      Printf.printf "Maximum cores: %d\n" max_cores;
      match l with
        | [] -> print_endline "No current task."
        | _ ->
          List.iter
            (fun c ->
              let time = Unix.gettimeofday() -. c.M.current_start in
              let job = c.M.current_job in
              Printf.printf
                "current job (cores %d, user %s, pid %d, cwd %s, issued %.2fs ago%s, running for %.2fs): %s\n"
                job.M.cores
                (maybe_str job.M.user)
                job.M.pid (maybe_str job.M.cwd)
                (now -. job.M.query_time)
                (tags2str job.M.tags)
                time (maybe_str job.M.info)
            ) l;
      List.iter
        (fun wjob ->
           let job = wjob.M.waiting_job in
           Printf.printf "waiting job n°%d (cores %d, user %s, pid %d, cwd %s, issued %.2fs ago%s): %s\n"
             wjob.M.waiting_id
             job.M.cores
             (maybe_str job.M.user)
             job.M.pid (maybe_str job.M.cwd)
             (now -. job.M.query_time)
             (tags2str job.M.tags)
             (maybe_str job.M.info)
        ) waiting;
  with e ->
     Logs.err ~src:section (fun k->k "error: %s" (Printexc.to_string e));
     ()

let main params =
  (* TODO setup loggers
  Lwt_log.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ();
  if params.debug
    then Lwt_log.add_rule "*" Lwt_log.Debug;
  *)
  match params.cmd with
  | PrintStatus -> print_status params
  | StopAccepting -> FrogLockClient.stop_accepting params.port
  | Exec _
  | Shell _ ->
      let res = run_command params in
      match res with
      | None -> ()
      | Some res ->
        (* TODO: print more details, like return code *)
        Logs.info ~src:section
          (fun k->k "process ran in %.2fs (pid: %d)" res.time res.pid);
        ()

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
    let doc = "Number of cores to lock for this task. If set to 0, then the
               task will lock all the cores." in
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
    `S "CONFIGURATION FILE";
    `P "Froglock uses a global configuration file located at $(b,/etc/froglock.conf). It should be composed
        of a line for each parameter to be set, of the form: 'parameter = value'. Currently accepted parameters are
        the following.";
    `I ("$(b,cores)", "maximum number of cores to use at any given time");
  ] in
  Term.(pure main $ (common_opts $ (pure aux $ shell $ cmd))),
  Term.info ~man ~doc "froglock"

let () =
  match Cmdliner.Term.eval_choice term [status_term; stop_term] with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok () -> ()

