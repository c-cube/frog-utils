
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

(* run the job's command on every argument, call [yield_res] with
  every result *)
let map_args ?timeout ~j cmd yield_res args =
  assert (j >= 1);
  (* use a pool to limit parallelism to [j] *)
  let pool = Lwt_pool.create j (fun () -> Lwt.return_unit) in
  Lwt_list.iter_p
    (fun arg ->
      Lwt_pool.use pool
        (fun () ->
          run_cmd ?timeout cmd arg >>= fun res ->
          yield_res res  (* output result *)
        )
    ) args

(* TODO: lock result file *)

(** {2 Main Commands} *)

module StrSet = Set.Make(String)

(* resume job from the given file *)
let resume ?timeout ~j file =
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
  Lwt_log.ign_debug_f "resume: %d remaining tasks (%d done)"
    (List.length remaining_tasks) (StrSet.cardinal done_tasks);
  S.append_job ~file
    (fun yield_res ->
      map_args ?timeout ~j job.S.cmd yield_res remaining_tasks
    )

let run_map params cmd args =
  (* chose output file *)
  ( match params.filename with
    | None -> S.make_fresh_file ?dir:params.dir "frogmapXXXXX"
    | Some f -> Lwt.return f
  ) >>= fun file ->
  Lwt_log.ign_debug_f "run command '%s' on %d arguments, parallelism %d"
    cmd (List.length args) params.parallelism_level;
  let job = {S.cmd = cmd; arguments=args; cwd= Sys.getcwd(); } in
  (* open file *)
  S.make_job ~file job
    (fun yield_res ->
      (* map [cmd] on every element of [args] *)
      map_args ?timeout:params.timeout ~j:params.parallelism_level
        cmd yield_res args
    )

let main params =
  match params.cmd with
  | Run (cmd, args) ->
      run_map params cmd args
  | Resume file ->
      resume ?timeout:params.timeout ~j:params.parallelism_level file

(** {2 Main} *)

let cmd_ = ref None
let j_ = ref 1
let args_ = ref []
let file_ = ref None
let dir_ = ref None
let timeout_ = ref None
let resume_ = ref None

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

let usage =
  "map [options] <file> cmd [--] arg [arg...] \
  map [options] -resume <file>"

let options = Arg.align
  [ "-j", Arg.Set_int j_, " parallelism level"
  ; "-o", Arg.String set_file_, " set state file"
  ; "-d", Arg.String set_dir_, " directory where to put state file"
  ; "-timeout", Arg.Float set_timeout_, " timeout for the command (in s)"
  ; "-debug", Arg.Unit set_debug_, " enable debug messages"
  ; "-resume", Arg.String set_resume_, " resume given file"
  ; "--", Arg.Rest push_, " arguments to the command"
  ]

(* TODO: handle "resume" *)

let () =
  Arg.parse options push_ usage;
  let mk_params cmd = {
    cmd;
    filename= !file_;
    dir= !dir_;
    parallelism_level= !j_;
    timeout = !timeout_;
  }
  in
  let params = match !resume_, !cmd_ with
    | Some f, _ -> mk_params (Resume f)
    | None, Some c -> mk_params (Run (c, List.rev !args_))
    | None, None -> Arg.usage options usage; exit 1
  in
  Lwt_main.run (main params)
