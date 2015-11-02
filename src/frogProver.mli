
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Prover}

  Utils to run a theorem prover (or a similar tool) and extract its result
*)

module StrMap : module type of Map.Make(String)

type env = (string * string) array

type t = {
  cmd : string;
  unsat : string option;
  sat : string option;
  unknown : string option;
  timeout : string option;
}

val make_command :
  ?env:env ->
  t ->
  timeout:int ->
  memory:int ->
  file:string ->
  string
(** Build a shell command that is ready to be executed, and that will
    run the given prover with the given parameters (time, input) *)

val build_from_config : FrogConfig.t -> string -> t
val find_config : FrogConfig.t -> string -> t
val of_config : FrogConfig.t -> t StrMap.t

val run_exec : ?env:env -> ?timeout:int -> ?memory:int ->
               config:FrogConfig.t ->
               prover:string ->
               file:string ->
               unit -> 'a
(** [run_exec ~config ~prover ~file ()] runs the prover named [prover] on the
    given [file]. How to run the prover is provided by [config].
    This uses {!Unix.execv} and therefore doesn't return. *)

val run_cmd : ?env:env -> ?timeout:int -> ?memory:int ->
               config:FrogConfig.t ->
               prover:string ->
               file:string ->
               (string * string array)
(** Same as {!run_exec}, but rather than executing the command, only
    returns a pair [(command, args)] that can be executed (starts with "sh") *)

(** {2 TPTP Provers} *)

module TPTP : sig

  val make_command :
    ?tptp:string ->
    t ->
    timeout:int ->
    memory:int ->
    file:string ->
    string
  (** Version of {!Prover.make_command} specialized to take the [tptp] env
      variable *)

  val run_cmd : ?tptp:string -> ?timeout:int -> ?memory:int ->
                 config:FrogConfig.t ->
                 prover:string ->
                 file:string ->
                 (string * string array)
  (** Version of {!run_cmd} that also looks into [config] for a
      key named "TPTP". *)

end
