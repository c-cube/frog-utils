
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Persistent State for FrogMap} *)

(** {2 Types} *)

type job = {
  cmd        : string [@key "cmd"];
  arguments  : string list [@key "arguments"];
  cwd        : string [@key "cwd"];
} [@@deriving yojson,show]
(** Description of a job *)

type result = {
  res_arg     : string [@key "arg"];
  res_rtime   : float [@key "time"];
  res_utime   : (float [@default 0.]) [@key "utime"];
  res_stime   : (float [@default 0.]) [@key "stime"];
  res_errcode : int [@key "errcode"];
  res_out     : string [@key "stdout"];
  res_err     : string [@key "stderr"];
} [@@deriving yojson {strict=false},show]
(** Result of running the command on one argument *)

(** {2 Creating/Updating State} *)

type yield_res = result -> unit
(** A function used to yield a new result *)

val make_fresh_file : ?dir:string -> string -> string
(** [make_fresh_file pattern] creates a fresh file with the given name
    pattern.
    @param dir directory into which to put the file (default: cwd) *)

val make_job : file:string -> job -> (yield_res -> 'a) -> 'a
(** [make_job ~file job f] creates a new state file for the given job,
    in file [file]. The content of [file] is erased.
    It calls [f] with a value that can be used to push results, *)

val append_job : file:string -> (yield_res -> 'a) -> 'a
(** [append_job ~file f] opens the file [file], expecting it to be
    a proper job file *)

(** {2 Read state} *)

val fold_state_s : ('a -> result -> 'a) -> (job -> 'a)
                  -> string -> 'a
(** [fold_state f init filename] opens the job's state saved
    in file named [filename], calls [init job] with the job itself,
    and fold through all results for this job using [f].
    @raise Failure if the file could not be found or if it's not a valid
      job file *)

val fold_state : ('a -> result -> 'a) -> (job -> 'a)
                  -> string -> 'a
(** Pure version of {!fold_state_s}, without theads.
    @raise Failure if the file could not be found or if it's not a valid
      job file *)

module StrMap : module type of Map.Make(String)

val read_state : string -> (job * result StrMap.t)
(** [read_state filename] opens [filename], and collects its results into
    an arg-indexed map. It also returns the corresponding job.
    @raise Failure if filename is not a proper job file *)

val write_state : string -> (job * result StrMap.t) -> unit
(** [write_state filename (job,res)] writes the result into the file,
    creating it if it didn't exist, overwriting it otherwise *)
