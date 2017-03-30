(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Config File} *)

exception Error of string
exception FieldNotFound of string * string

type t

val empty : t

val create : unit -> t
(** Local config file *)

val interpolate_home : string -> string
(** Interpolate $HOME in the given string *)

val parse_or_empty : string -> t
(** Parse a config file or return an empty one *)

val parse_files : string list -> t -> t
(** Merge the given config files (in increasing priority order) into
    the given config. *)

(** {2 Accessors}

Accessors will raise FieldNotFound if the value is unreachable AND
no default is specified *)

type 'a getter = ?default:'a -> t -> string -> 'a

val get_table : t getter
val get_bool : bool getter
val get_int : int getter
val get_float : float getter
val get_string : string getter
val get_string_list : string list getter

val get_string_opt : t -> string -> string option
(** never fails, return [None] if not found *)

(* TODO: other wrappers *)
