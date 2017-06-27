
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Map a command on many arguments, with parallelism} *)
open Frog
open Frog_server

module S = FrogMapState

type cmd =
  | Resume of string list       (* resume filename *)
  | Run of string * string list (* run command *)

type params = {
  cmd : cmd;
  filename : string option;
  dir : string option;
  parallelism_level : int;
  timeout : float option;
  progress : bool;
  lock : bool;
  port : int;
  priority : int;
}

(** {2 Processing} *)

(* TODO: resume by reading job, diff with already done jobs, chdir, map_args
  Sys.chdir params.job.S.cwd;
  *)

(* run command on the argument, return a [S.result] *)
let run_cmd ?timeout cmd arg =
  let cmd' = cmd ^ " " ^ arg |> Lwt_process.shell in
  Lwt_log.ign_debug_f "start running '%s' on '%s'" cmd arg;
  let start = Unix.gettimeofday () in
  Lwt_process.with_process_full ?timeout cmd'
    (fun p ->
      let%lwt () = Lwt_io.close p#stdin
      and res_out = Lwt_io.read p#stdout
      and res_err = Lwt_io.read p#stderr
      and res_errcode = Lwt.map
        (function
          | Unix.WEXITED e -> e
          | Unix.WSIGNALED _
          | Unix.WSTOPPED _  -> 128
        ) p#status
      in
      let res_rtime = Unix.gettimeofday () -. start in
      let%lwt rusage = p#rusage in
      let res_utime = rusage.Lwt_unix.ru_utime in
      let res_stime = rusage.Lwt_unix.ru_stime in
      Lwt_log.ign_debug_f "process '%s' on '%s': done (real : %.2fs, user : %.2f, system : %.2f)"
        cmd arg res_rtime res_utime res_stime;
      Lwt.return {S.res_arg=arg; res_rtime; res_utime; res_stime; res_errcode; res_out; res_err; }
    )

let with_lock ~daemon ~priority f info =
  let cwd = Sys.getcwd () in
  let user = try Some(Sys.getenv "USER") with _ -> None in
  FrogLockClient.acquire ~cwd ?user ~info ~cores:1 ~priority daemon
    (function
      | true ->
        Lwt_log.ign_debug_f "hurray ! lock acquired";
        let%lwt x = f () in
        Lwt.return (Some x)
      | false ->
        Lwt_log.ign_debug_f "hu ho... didn't get the lock...";
        Lwt.return_none
    )

(* thread that prints progress *)
let nb_sec_minute = 60
let nb_sec_hour = 60 * nb_sec_minute
let nb_sec_day = 24 * nb_sec_hour

let time_string f =
  let n = int_of_float f in
  let aux n div = n / div, n mod div in
  let n_day, n = aux n nb_sec_day in
  let n_hour, n = aux n nb_sec_hour in
  let n_min, n = aux n nb_sec_minute in
  let print_aux s n = if n <> 0 then (string_of_int n) ^ s else "" in
  (print_aux "d" n_day) ^
  (print_aux "h" n_hour) ^
  (print_aux "m" n_min) ^
  (string_of_int n) ^ "s"

let make_progress_thread n =
  let cur = ref 0 in
  let start = Unix.gettimeofday () in
  let rec loop () =
    let time_elapsed = Unix.gettimeofday () -. start in
    let len_bar = 30 in
    let bar = String.init len_bar (fun i -> if i * n <= len_bar * !cur then '#' else ' ') in
    let percent = if n=0 then 100 else (!cur * 100) / n in
    let%lwt () = Lwt_io.printf "\r... %5d/%d | %3d%% [%6s: %s]"
      !cur n percent (time_string time_elapsed) bar
    in
    let%lwt () = Lwt_io.flush Lwt_io.stdout in
    if !cur = n
    then
      let%lwt () = Lwt_io.printl "" in
      Lwt_io.flush Lwt_io.stdout
    else
      let%lwt () = Lwt_unix.sleep 0.2 in
      loop ()
  in
  (fun () -> incr cur), (loop ())

(* run the job's command on every argument, call [yield_res] with
  every result *)
let map_args_aux ?daemon ?timeout ~priority ~progress ~j cmd yield_res args =
  assert (j >= 1);
  let send_done, progress_thread = if progress
    then make_progress_thread (List.length args)
    else (fun () -> ()), Lwt.return_unit
  in
  (* use a pool to limit parallelism to [j] *)
  let pool = Lwt_pool.create j (fun () -> Lwt.return_unit) in
  let iter_thread = Lwt_list.iter_p
    (fun arg ->
      Lwt_pool.use pool
        (fun () ->
          Lwt_log.ign_debug_f "run on %s..." arg;
          let%lwt res =
            match daemon with
            | Some daemon -> with_lock ~daemon ~priority (fun () -> run_cmd ?timeout cmd arg) (cmd ^ " " ^ arg)
            | None -> Lwt.map (fun x -> Some x) (run_cmd ?timeout cmd arg)
          in
          match res with
          | Some res ->
            send_done ();
            Lwt_log.ign_debug_f "... %s: done (errcode %d)" arg res.S.res_errcode;
            yield_res res  (* output result *)
          | None -> assert false (* Retry doing the computation ? *)
        )
    ) args
  in
  Lwt.join [progress_thread; iter_thread]

let map_args ?timeout ~lock ~port ~priority ~progress ~j cmd yield_res args =
  if lock then
    FrogLockClient.connect_or_spawn port (fun daemon ->
        map_args_aux ~daemon ?timeout ~priority ~progress ~j cmd yield_res args)
  else
        map_args_aux ?timeout ~priority ~progress ~j cmd yield_res args

(* TODO: lock result file *)

(** {2 Main Commands} *)

module StrSet = Misc.StrSet

(* resume job from the given file *)
let resume ?timeout ~lock ~port ~priority ~progress ~j file =
  (* get the job and the set of executed tasks *)
  let%lwt (job, done_tasks) =
    S.fold_state
      (fun (job,set) res -> job, StrSet.add res.S.res_arg set)
      (fun job -> job, StrSet.empty)
      file
  in
  let remaining_tasks = List.filter
    (fun arg -> not (StrSet.mem arg done_tasks))
    job.S.arguments
  in
  (* change directory *)
  Lwt_log.ign_debug_f "change directory to %s" job.S.cwd;
  Sys.chdir job.S.cwd;
  (* execute remaining tasks *)
  let%lwt () = Lwt_io.printlf "resume %s: %d remaining tasks (%d done)"
    file (List.length remaining_tasks) (StrSet.cardinal done_tasks) in
  S.append_job ~file
    (fun yield_res ->
       map_args ?timeout ~lock ~port ~priority
         ~progress ~j job.S.cmd yield_res remaining_tasks
    )

let run_map params cmd args =
  (* chose output file *)
  let%lwt file = match params.filename with
    | None -> S.make_fresh_file ?dir:params.dir "frogmapXXXXX.json"
    | Some f -> Lwt.return f
  in
  let%lwt () = Lwt_io.printlf
    "run command '%s' on %d arguments, parallelism %d, output to %s"
    cmd (List.length args) params.parallelism_level file in
  let job = {S.cmd = cmd; arguments=args; cwd= Sys.getcwd(); } in
  (* open file *)
  S.make_job ~file job
    (fun yield_res ->
       (* map [cmd] on every element of [args] *)
       map_args ?timeout:params.timeout
         ~lock:params.lock
         ~port:params.port
         ~priority:params.priority
         ~progress:params.progress
         ~j:params.parallelism_level
         cmd yield_res args
    )

let main params =
  match params.cmd with
  | Run (cmd, args) ->
      run_map params cmd args
  | Resume files ->
    Lwt_list.iter_s
      (resume ?timeout:params.timeout
         ~lock:params.lock
         ~port:params.port
         ~priority:params.priority
         ~progress:params.progress
         ~j:params.parallelism_level)
      files

(** {2 Main} *)

let read_file_args file =
  Lwt_io.with_file ~mode:Lwt_io.input file
    (fun ic ->
      let lines = Lwt_io.read_lines ic in
      Lwt_stream.to_list lines
    )

let opts =
  let open Cmdliner in
  let aux debug dir j timeout progress lock port priority file cmd =
    Lwt_log.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ();
    if debug then
      Lwt_log.add_rule "*" Lwt_log.Debug;
    let progress = progress && not debug in
    let%lwt cmd = cmd in
    Lwt.return { cmd; filename = file; dir; parallelism_level = j; timeout; progress;
                 lock; port; priority }
  in
  let debug =
    let doc = "Enable debug mode" in
    Arg.(value & flag & info ["d"; "debug"] ~doc)
  in
  let dir =
    let doc = "Directory where to put the state file" in
    Arg.(value & opt (some dir) None & info ["d"; "dir"] ~docv:"DIR" ~doc)
  in
  let j =
    let doc = "Number of parralel process to use" in
    Arg.(value & opt int 1 & info ["j"] ~docv:"N" ~doc)
  in
  let timeout =
    let doc = "Timeout" in
    Arg.(value & opt (some float) None & info ["t"; "timeout"] ~docv:"TIME" ~doc)
  in
  let progress =
    let doc = "Enable/diable progress bar" in
    Arg.(value & opt bool true & info ["b"; "progress"] ~docv:"BOOL" ~doc)
  in
  let lock =
    let doc = "Set wether to encapsulates each call to the command in a froglock." in
    Arg.(value & opt bool true & info ["lock"] ~docv:"BOOL" ~doc)
  in
  let port =
    let doc = "Connection port for the daemon (see froglock for more explanation)." in
    Arg.(value & opt int IPC_daemon.default_port & info ["p"; "port"] ~docv:"PORT" ~doc)
  in
  let prio =
    let doc = "Priority for the locked task (see froglock for more explanation)." in
    Arg.(value & opt int 10 & info ["prio"] ~docv:"PRIO" ~doc)
  in
  Term.(pure aux $ debug $ dir $ j $ timeout $ progress $ lock $ port $ prio)

let resume_term =
  let open Cmdliner in
  let cmd l = Lwt.return (Resume l) in
  let aux params = Lwt_main.run (Lwt.bind params main) in
  let file =
    let doc = "Task file to be resumed" in
    Arg.(non_empty & pos_all non_dir_file [] & info [] ~docv:"FILE" ~doc)
  in
  let doc = "Resume an previous instance of frogmap using its output file." in
  let man = [
    `S "DESCRIPTION";
    `P "Resume an instance of frogmap, using the output file of the previous instance
        in order to continue where the previous instance was stopped. Tasks that will thus be run are
        those that were running and those that were waiting to be run when the previous instance stopped.";
    `P "Options such as parralelisme degree, timeout,... are $(b,NOT) stored in the output file, and thus can be modified
        from one execution to another.";
  ] in
  Term.(pure aux $ (opts $ pure None $ (pure cmd $ file))),
  Term.info ~man ~doc "resume"

let term =
  let open Cmdliner in
  let params cmd args file_args =
    let%lwt args = match file_args with
      | None -> Lwt.return args
      | Some f -> read_file_args f
    in
    Lwt.return (Run (cmd, args))
  in
  let aux params = Lwt_main.run (Lwt.bind params main) in
  let file =
    let doc = "Output file" in
    Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"FILE" ~doc)
  in
  let cmd =
    let doc = "Command to be mapped" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"CMD" ~doc)
  in
  let args =
    let doc = "Arguments on which to map the given command" in
    Arg.(value & pos_right 0 string [] & info [] ~docv:"ARGS" ~doc)
  in
  let file_args =
    let doc = "Read arguments from file" in
    Arg.(value & opt (some non_dir_file) None & info ["F"] ~docv:"FILE" ~doc)
  in
  let doc = "Maps the given commands on the given inputs." in
  let man = [
    `S "SYNOPSIS";
    `I ("$(b,frogmap COMMAND)", "Call the command");
    `I ("$(b,frogmap [OPTIONS] -- CMD [ARGS [ARGS...]])", "Mapp the command on the list of arguments.");
    `S "DESCRIPTION";
    `P "Maps the given command on a list of inputs, and store the results in
        a file (in json format). The file can later be analysed using the 'frogiter' command.
        Also allows to parallelize the given task using multipls threads.";
  ] in
  Term.(pure aux $ (opts $ file $ (pure params $ cmd $ args $ file_args))),
  Term.info ~man ~doc "frogmap"

let () =
  match Cmdliner.Term.eval_choice term [resume_term] with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok () -> ()

