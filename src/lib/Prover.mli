
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Prover}

    Utils to run a theorem prover (or a similar tool) and extract its result
*)

(** {2 Prover configurations} *)

module StrMap : module type of Map.Make(String)
(** Maps indexed by strings. *)

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

val of_config : FrogConfig.t -> t StrMap.t
(** Get a list of supported provers from a config file. *)

val build_from_config : FrogConfig.t -> string -> t
(** Parse the description of a prover from a config file *)

val find_config : FrogConfig.t -> string -> t
(** Parse prover description from config file, and check it is listed
    in the "provers" list *)

val make_command :
  ?env:(string * string) array ->
  t ->
  timeout:int ->
  memory:int ->
  file:string ->
  string

val hash : t -> string
val db_init : FrogDB.t -> unit
val db_add : FrogDB.t -> t -> unit
val find : FrogDB.t -> string -> t option
val find_all : FrogDB.t -> t list

val name : t -> string

val to_html_name : t -> FrogWeb.html
val to_html_fullname : t -> FrogWeb.html
val to_html_full : t -> FrogWeb.html

val k_uri : (t -> Uri.t) FrogWeb.HMap.key
val k_add : (t -> unit) FrogWeb.HMap.key
val add_server : FrogWeb.Server.t -> unit

