
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Prover}

    Utils to run a theorem prover (or a similar tool) and extract its result
*)

(** {2 Prover configurations} *)

module StrMap = Misc.StrMap

type version =
  | Tag of string
  | Git of string * string  (* branch & commit hash *)
  [@@deriving yojson]

type t = {
  (* Prover identification *)
  name : string;
  version : version;

  (* Pover execution *)
  binary: string;       (* name of the program itself *)
  cmd: string;          (* the command line to run.
                           possibly contains $binary, $file, $memory and $timeout *)

  (* Result analysis *)
  unsat   : string option;  (* regex for "unsat" *)
  sat     : string option;  (* regex for "sat" *)
  unknown : string option;  (* regex for "unknown" *)
  timeout : string option;  (* regex for "timeout" *)
  memory  : string option;  (* regex for "out of memory" *)
} [@@deriving yojson]
(** The type of provers configurations *)

val equal : t -> t -> bool
(** Equality (by name) *)

val maki : t Maki.Value.ops
(** Maki values for provers. *)

val of_config : Config.t -> t StrMap.t
(** Get a list of supported provers from a config file. *)

val build_from_config : Config.t -> string -> t
(** Parse the description of a prover from a config file *)

val find_config : Config.t -> string -> t
(** Parse prover description from config file, and check it is listed
    in the "provers" list *)

val make_command :
  ?env:(string * string) array ->
  t ->
  timeout:int ->
  memory:int ->
  file:string ->
  string

val name : t -> string
val hash : t -> string

val to_html_name : t -> Web.html
val to_html_fullname : t -> Web.html
val to_html_full : t -> Web.html

