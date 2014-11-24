
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

(** {1 Persistent State for FrogMap} *)

(** {2 Types} *)

type job = {
  cmd       [@key "cmd"]        : string;
  arguments [@key "arguments"]  : string list;
  cwd       [@key "cwd"]        : string;
} [@@deriving yojson,show]
(** Description of a job *)

type result = {
  res_arg     [@key "arg"]     : string;
  res_time    [@key "time"]    : float;
  res_errcode [@key "errcode"] : int;
  res_out     [@key "stdout"]  : string;
  res_err     [@key "stderr"]  : string;
} [@@deriving yojson,show]
(** Result of running the command on one argument *)

(** {2 Creating/Updating State} *)

type yield_res = result -> unit Lwt.t
(** A function used to yield a new result *)

val make_fresh_file : ?dir:string -> string -> string Lwt.t
(** [make_fresh_file pattern] creates a fresh file with the given name
    pattern.
    @param dir directory into which to put the file (default: cwd) *)

val make_job : file:string -> job -> (yield_res -> 'a Lwt.t) -> 'a Lwt.t
(** [make_job ~file job f] creates a new state file for the given job,
    in file [file]. The content of [file] is erased.
    It calls [f] with a value that can be used to push results, *)

val append_job : file:string -> (yield_res -> 'a Lwt.t) -> 'a Lwt.t
(** [append_job ~file f] opens the file [file], expecting it to be
    a proper job file
    TODO: check this property and fail if needed *)

(** {2 Read state} *)

val fold_state_s : ('a -> result -> 'a Lwt.t) -> (job -> 'a Lwt.t)
                  -> string -> 'a Lwt.t
(** [fold_state f init filename] opens the job's state saved
    in file named [filename], calls [init job] with the job itself,
    and fold through all results for this job using [f].
    @raise Failure if the file could not be found or if it's not a valid
      job file *)

val fold_state : ('a -> result -> 'a) -> (job -> 'a)
                  -> string -> 'a Lwt.t
(** Pure version of {!fold_state_s}, without theads.
    @raise Failure if the file could not be found or if it's not a valid
      job file *)

module StrMap : module type of Map.Make(String)

val read_state : string -> (job * result StrMap.t) Lwt.t
(** [read_state filename] opens [filename], and collects its results into
    an arg-indexed map. It also returns the corresponding job.
    @raise Failure if filename is not a proper job file *)
