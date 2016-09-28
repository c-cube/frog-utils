
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Simple wrapper for HTML} *)

include module type of Tyxml.Html5

type t = Html_types.div_content_fun elt
type html = t

val to_string : _ Tyxml.Html5.elt -> string

(** {2 Encoding Records in HTML} *)

module Record : sig
  type t
  val start : t
  val add               : string -> html -> t -> t
  val add_int           : ?raw:bool -> string -> int -> t -> t
  val add_float         : ?raw:bool -> string -> float -> t -> t
  val add_string        : ?raw:bool -> string -> string -> t -> t
  val add_string_option : ?raw:bool -> string -> string option -> t -> t
  val add_bool          : ?raw:bool -> string -> bool -> t -> t
  val close : t -> html
end

(* TODO: same as record, but for full tables? *)

