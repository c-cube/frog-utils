
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

(** {1 Map a command on many arguments, with parallelism} *)

module S = FrogMapState

let (>>=) = Lwt.(>>=)

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
      Lwt_io.close p#stdin >>= fun () ->
      let out = Lwt_io.read p#stdout
      and err = Lwt_io.read p#stderr
      and errcode = Lwt.map
        (function
          | Unix.WEXITED e -> e
          | Unix.WSIGNALED _
          | Unix.WSTOPPED _  -> 128
        ) p#status
      in
      out >>= fun res_out ->
      err >>= fun res_err ->
      errcode >>= fun res_errcode ->
      let res_time = Unix.gettimeofday() -. start in
      Lwt_log.ign_debug_f "process '%s' on '%s': done (%.2fs)" cmd arg res_time;
      Lwt.return {S.res_arg=arg; res_time; res_errcode; res_out; res_err; }
    )

(* thread that prints progress *)
let make_progress_thread n =
  let cur = ref 0 in
  let start = Unix.gettimeofday () in
  let rec loop () =
    if !cur = n
    then Lwt_io.printl ""
    else (
      let time_elapsed = Unix.gettimeofday () -. start in
      let bar = String.init 20 (fun i -> if i * n < 20 * !cur then '#' else ' ') in
      Lwt_io.printf "\r... %d/%d [%.0fs: %s]" !cur n time_elapsed bar >>= fun () ->
      Lwt_io.flush Lwt_io.stdout >>= fun () ->
      Lwt_unix.sleep 0.2 >>= fun () ->
      loop ()
    )
  in
  Lwt.async loop;
  fun () -> incr cur

(* run the job's command on every argument, call [yield_res] with
  every result *)
let map_args ?timeout ~progress ~j cmd yield_res args =
  assert (j >= 1);
  let send_done = if progress
    then make_progress_thread (List.length args)
    else (fun () -> ())
  in
  (* use a pool to limit parallelism to [j] *)
  let pool = Lwt_pool.create j (fun () -> Lwt.return_unit) in
  Lwt_list.iter_p
    (fun arg ->
      Lwt_pool.use pool
        (fun () ->
          Lwt_log.ign_debug_f "run on %s..." arg;
          run_cmd ?timeout cmd arg >>= fun res ->
          send_done ();
          Lwt_log.ign_debug_f "... %s: done (errcode %d)" arg res.S.res_errcode;
          yield_res res  (* output result *)
        )
    ) args

(* TODO: lock result file *)

(** {2 Main Commands} *)

module StrSet = Set.Make(String)

(* resume job from the given file *)
let resume ?timeout ~progress ~j file =
  (* get the job and the set of executed tasks *)
  S.fold_state
    (fun (job,set) res -> job, StrSet.add res.S.res_arg set)
    (fun job -> job, StrSet.empty)
    file
  >>= fun (job, done_tasks) ->
  let remaining_tasks = List.filter
    (fun arg -> not (StrSet.mem arg done_tasks))
    job.S.arguments
  in
  (* change directory *)
  Lwt_log.ign_debug_f "change directory to %s" job.S.cwd;
  Sys.chdir job.S.cwd;
  (* execute remaining tasks *)
  Lwt_io.printlf "resume: %d remaining tasks (%d done)"
    (List.length remaining_tasks) (StrSet.cardinal done_tasks) >>= fun () ->
  S.append_job ~file
    (fun yield_res ->
      map_args ?timeout ~progress ~j job.S.cmd yield_res remaining_tasks
    )

let run_map params cmd args =
  (* chose output file *)
  begin match params.filename with
    | None -> S.make_fresh_file ?dir:params.dir "frogmapXXXXX.json"
    | Some f -> Lwt.return f
  end >>= fun file ->
  Lwt_io.printlf "run command '%s' on %d arguments, parallelism %d, output to %s"
    cmd (List.length args) params.parallelism_level file >>= fun () ->
  let job = {S.cmd = cmd; arguments=args; cwd= Sys.getcwd(); } in
  (* open file *)
  S.make_job ~file job
    (fun yield_res ->
      (* map [cmd] on every element of [args] *)
      map_args ?timeout:params.timeout
        ~progress:params.progress
        ~j:params.parallelism_level
        cmd yield_res args
    )

let main params =
  match params.cmd with
  | Run (cmd, args) ->
      run_map params cmd args
  | Resume file ->
      resume ?timeout:params.timeout
        ~progress:params.progress
        ~j:params.parallelism_level
        file

(** {2 Main} *)

let cmd_ = ref None
let j_ = ref 1
let args_ = ref []
let file_ = ref None
let dir_ = ref None
let timeout_ = ref None
let resume_ = ref None
let file_args_ = ref None
let progress_ = ref true

let set_file_ s = file_ := Some s
let set_dir_ s = dir_ := Some s
let push_ s = match !cmd_ with
  | None -> cmd_ := Some s
  | Some _ -> args_ := s :: !args_
let set_timeout_ f =
  if f <= 0. then failwith "timeout must be > 0"
  else timeout_ := Some f
let set_debug_ () =
  Lwt_log.add_rule "*" Lwt_log.Debug
let set_resume_ s = match !resume_ with
  | Some _ -> failwith "can resume at most one file"
  | None -> resume_ := Some s
let set_file_args_ f = file_args_ := Some f

let usage =
  "map [options] <file> cmd [--] arg [arg...] \n\
  map [options] -resume <file>"

let options = Arg.align
  [ "-j", Arg.Set_int j_, " parallelism level"
  ; "-o", Arg.String set_file_, " set state file"
  ; "-d", Arg.String set_dir_, " directory where to put state file"
  ; "-progress", Arg.Bool (fun b->progress_ := b), " enable/disable progress bar"
  ; "-timeout", Arg.Float set_timeout_, " timeout for the command (in s)"
  ; "-debug", Arg.Unit set_debug_, " enable debug messages"
  ; "-resume", Arg.String set_resume_, " resume given file"
  ; "-F", Arg.String set_file_args_, " read arguments from file"
  ; "--", Arg.Rest push_, " arguments to the command"
  ]

let read_file_args file =
  Lwt_io.with_file ~mode:Lwt_io.input file
    (fun ic ->
      let lines = Lwt_io.read_lines ic in
      Lwt_stream.to_list lines
    )

let read_params () =
  let mk_params cmd =
    Lwt.return {
      cmd;
      filename= !file_;
      dir= !dir_;
      parallelism_level= !j_;
      timeout = !timeout_;
      progress = !progress_;
    }
  in
  match !resume_, !cmd_ with
    | Some f, _ -> mk_params (Resume f)
    | None, Some c ->
        begin match !file_args_ with
          | None -> Lwt.return (List.rev !args_)
          | Some f -> read_file_args f
        end >>= fun args ->
        mk_params (Run (c, args))
    | None, None -> Arg.usage options usage; exit 1

let () =
  Arg.parse options push_ usage;
  Lwt_main.run (read_params () >>= main)
