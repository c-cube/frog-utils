
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Map a command on many arguments, with parallelism} *)

module S = FrogMapState

let section = Logs.Src.create "frogmap"

type cmd =
  | Resume of string   (* resume filename *)
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

let kill_after_ ?timeout p = match timeout with
  | None -> ()
  | Some t ->
      CCThread.detach
        (fun () ->
          Thread.delay t;
          ignore p#close)

(* run command on the argument, return a [S.result] *)
let run_cmd ?timeout cmd arg =
  let cmd' = cmd ^ " " ^ arg in
  Logs.debug ~src:section (fun k->k "start running '%s' on '%s'" cmd arg);
  let start = Unix.gettimeofday () in
  CCUnix.with_process_full cmd'
    ~f:(fun p ->
      kill_after_ ?timeout p;
      close_out p#stdin;
      let res_out = CCIO.read_all p#stdout
      and res_err = CCIO.read_all p#stderr in
      let res_errcode = match p#close with
          | Unix.WEXITED e -> e
          | Unix.WSIGNALED _
          | Unix.WSTOPPED _  -> 128
      in
      let res_rtime = Unix.gettimeofday () -. start in
      let res_utime = 0. and res_stime = 0. in
      (* FIXME
      let rusage = p#rusage in
      let res_utime = rusage.Lwt_unix.ru_utime in
      let res_stime = rusage.Lwt_unix.ru_stime in
      *)
      Logs.debug ~src:section (fun k->
        k "process '%s' on '%s': done (real : %.2fs, user : %.2f, system : %.2f)"
        cmd arg res_rtime res_utime res_stime);
      {S.res_arg=arg; res_rtime; res_utime; res_stime; res_errcode; res_out; res_err; }
    )

let with_lock ~daemon ~priority f info =
  let cwd = Sys.getcwd () in
  let user = try Some(Sys.getenv "USER") with _ -> None in
  FrogLockClient.acquire ~cwd ?user ~info ~cores:1 ~priority daemon
    (function
      | true ->
        Logs.debug ~src:section (fun k->k "hurray ! lock acquired");
        let x = f () in
        Some x
      | false ->
        Logs.debug ~src:section (fun k->k "hu ho... something wrong happened...");
        None
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
    Printf.printf "\r... %5d/%d | %3d%% [%6s: %s]%!"
      !cur n percent (time_string time_elapsed) bar;
    if !cur = n
    then (
      print_endline "";
      flush stdout;
    ) else (
      Thread.delay 0.2;
      loop ()
    )
  in
  (fun () -> incr cur), CCThread.spawn loop

(* run the job's command on every argument, call [yield_res] with
  every result *)
let map_args_aux ?daemon ?timeout ~priority ~progress ~j cmd yield_res args =
  assert (j >= 1);
  let send_done, _progress_thread = if progress
    then make_progress_thread (List.length args)
    else (fun () -> ()), Thread.create CCFun.id ()
  in
  (* TODO use a thread pool of [j] elements *)
  List.iter
    (fun arg ->
      (* TODO synchronize, we should wait for the thread to stop *)
      CCThread.detach
        (fun () ->
          Logs.debug ~src:section (fun k->k "run on %s..." arg);
          let res =
            match daemon with
            | Some daemon ->
                with_lock ~daemon ~priority
                  (fun () -> run_cmd ?timeout cmd arg) (cmd ^ " " ^ arg)
            | None -> Some (run_cmd ?timeout cmd arg)
          in
          match res with
          | Some res ->
            send_done ();
            Logs.debug ~src:section
              (fun k->k "... %s: done (errcode %d)" arg res.S.res_errcode);
            yield_res res  (* output result *)
          | None -> assert false (* Retry doing the computation ? *)
        ))
    args

let map_args ?timeout ~lock ~port ~priority ~progress ~j cmd yield_res args =
  if lock
  then
    FrogLockClient.connect_or_spawn port
      (fun daemon ->
        map_args_aux ~daemon ?timeout ~priority ~progress ~j cmd yield_res args)
  else
    map_args_aux ?timeout ~priority ~progress ~j cmd yield_res args

(* TODO: lock result file *)

(** {2 Main Commands} *)

module StrSet = Set.Make(String)

(* resume job from the given file *)
let resume ?timeout ~lock ~port ~priority ~progress ~j file =
  (* get the job and the set of executed tasks *)
  let job, done_tasks =
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
  Logs.debug ~src:section (fun k->k "change directory to %s" job.S.cwd);
  Sys.chdir job.S.cwd;
  (* execute remaining tasks *)
  Printf.printf "resume: %d remaining tasks (%d done)\n"
    (List.length remaining_tasks) (StrSet.cardinal done_tasks);
  S.append_job ~file
    (fun yield_res ->
       map_args ?timeout ~lock ~port ~priority
         ~progress ~j job.S.cmd yield_res remaining_tasks)

let run_map params cmd args =
  (* chose output file *)
  let file = match params.filename with
    | None -> S.make_fresh_file ?dir:params.dir "frogmapXXXXX.json"
    | Some f -> f
  in
  Printf.printf
    "run command '%s' on %d arguments, parallelism %d, output to %s\n"
    cmd (List.length args) params.parallelism_level file;
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
         cmd yield_res args)

let main params =
  match params.cmd with
  | Run (cmd, args) ->
      run_map params cmd args
  | Resume file ->
    resume ?timeout:params.timeout
      ~lock:params.lock
      ~port:params.port
      ~priority:params.priority
      ~progress:params.progress
      ~j:params.parallelism_level
      file

(** {2 Main} *)

let read_file_args file = CCIO.with_in file CCIO.read_lines_l

let opts =
  let open Cmdliner in
  let aux dir j timeout progress lock port priority file cmd =
    { cmd; filename = file; dir; parallelism_level = j; timeout; progress;
      lock; port; priority }
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
    Arg.(value & opt int 12000 & info ["p"; "port"] ~docv:"PORT" ~doc)
  in
  let prio =
    let doc = "Priority for the locked task (see froglock for more explanation)." in
    Arg.(value & opt int 10 & info ["prio"] ~docv:"PRIO" ~doc)
  in
  Term.(pure aux $ dir $ j $ timeout $ progress $ lock $ port $ prio)

let resume_term =
  let open Cmdliner in
  let cmd f = Resume f in
  let aux params = main params in
  let file =
    let doc = "Task file to be resumed" in
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)
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
    let args = match file_args with
      | None -> args
      | Some f -> read_file_args f
    in
    Run (cmd, args)
  in
  let aux params = main params in
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

