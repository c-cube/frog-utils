
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Tests} *)

open Frog

type dir = {
  directory : string;
  pattern : string;
  expect : ProblemSet.expect;
}

type config = {
  j : int;                       (* number of concurrent processes *)
  memory : int;                  (* memory limit for each problem *)
  timeout : int;                 (* timeout for each problem *)
  provers : Prover.t list;
  problems : dir list;
}

val mk_config : ?profile:string -> Config.t -> string list -> config Misc.Err.t
(** [config_of_config ?profile conf dirs] makes a test config out of the
    raw configuration.
    It will gather all problems present in one of the [dirs] that the
    configuration matches.
    @param profile if present, look for the test configuration named [profile]
    instead of "test" *)

val config_of_file : ?profile:string -> string -> config Misc.Err.t

val print_result : Event.prover Event.result -> unit

val run :
  ?on_dir:(dir -> string list -> unit Lwt.t) ->
  ?on_solve:(Event.prover Event.result -> unit Lwt.t) ->
  ?caching:bool -> config -> unit Misc.LwtErr.t
(** Run the given configuration.
    @param caching if true, use Maki for caching results (default true)
    @param on_solve called whenever a single problem is solved *)

