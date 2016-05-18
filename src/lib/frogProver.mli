
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Prover}

    Utils to run a theorem prover (or a similar tool) and extract its result
*)

(** {2 Prover configurations} *)

type html = FrogWeb.html

module StrMap : module type of Map.Make(String)
(** Maps indexed by strings. *)

type t = {
  binary: string; (* name of the program itself *)
  cmd: string;
  (* the command line to run.
     possibly contains $binary, $file, $memory and $timeout *)
  unsat : string option;
  sat : string option;
  unknown : string option;
  timeout : string option;
  memory : string option;
} [@@deriving yojson]
(** The type of provers configurations *)

val maki : t Maki.Value.ops
(** Maki values for provers. *)

val of_config : FrogConfig.t -> t StrMap.t
(** Get a list of supported provers from a config file. *)

val build_from_config : FrogConfig.t -> string -> t
(** Parse the description of a prover from a config file *)

val find_config : FrogConfig.t -> string -> t
(** Parse prover description from config file, and check it is listed
    in the "provers" list *)

val to_html_name : t -> html
val to_html_full : t -> html

val k_uri : (t -> Uri.t) FrogWeb.HMap.key
val k_add : (t -> unit) FrogWeb.HMap.key
val add_server : FrogWeb.Server.t -> unit

(** {2 Prover commands} *)

type env = (string * string) array
(** Shell Environnments *)

val make_command :
  ?env:env ->
  t ->
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
  prover:t ->
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
  prover:t ->
  file:string ->
  unit ->
  (string * string array)
(** Same as {!run_exec}, but rather than executing the command, only
    returns a pair [(command, args)] that can be executed (starts with "sh") *)

val run_proc :
  ?env:env ->
  timeout:int ->
  memory:int ->
  prover:t ->
  file:string ->
  unit ->
  (string * string * int) Lwt.t
(** Runs the prover in a sub-process, and returns a tuple
    [stdout, stderr, errcode] *)

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
