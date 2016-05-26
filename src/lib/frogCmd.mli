
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {2 Prover commands} *)

type env = (string * string) array
(** Shell Environnments *)

val make_command :
  ?env:env ->
  FrogProver.t ->
  timeout:int ->
  memory:int ->
  file:string ->
  string
(** Build a shell command that is ready to be executed, and that will
    run the given prover with the given parameters (time, input) *)

val run_exec :
  ?env:env ->
  timeout:int ->
  memory:int ->
  prover:FrogProver.t ->
  file:string ->
  unit -> 'a
(** [run_exec ~timeout ~memory ~config ~prover ~file ()] runs the
    prover named [prover] on the given [file]. How to run the prover
    is provided by [config]. This uses {!Unix.execv} and therefore
    doesn't return. *)

val run_cmd :
  ?env:env ->
  timeout:int ->
  memory:int ->
  prover:FrogProver.t ->
  file:string ->
  unit ->
  (string * string array)
(** Same as {!run_exec}, but rather than executing the command, only
    returns a pair [(command, args)] that can be executed (starts with "sh") *)

val run_proc :
  ?env:env ->
  timeout:int ->
  memory:int ->
  prover:FrogProver.t ->
  pb:FrogProblem.t ->
  unit ->
  FrogMap.raw_result Lwt.t
(** Runs the prover in a sub-process, and returns a tuple
    [stdout, stderr, errcode] *)

(** {2 TPTP Provers} *)

module TPTP : sig
  val make_command :
    ?tptp:string ->
    FrogProver.t ->
    timeout:int ->
    memory:int ->
    file:string ->
    string
  (** Version of {!Prover.make_command} specialized to take the [tptp] env
      variable *)

  val run_cmd :
    ?tptp:string ->
    timeout:int ->
    memory:int ->
    config:FrogConfig.t ->
    prover:string ->
    file:string ->
    unit ->
    (string * string array)
    (** Version of {!run_cmd} that also looks into [config] for a
        key named "TPTP". *)
end
