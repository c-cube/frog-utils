
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(* TODO: remove and replace wiht Logs *)

(** {1 Debug function} *)

val set_debug : bool -> unit
(** Enable or disable debug *)

val enable_debug : unit -> unit

val log : ?section:string -> ('a, Format.formatter, unit, unit) format4 -> 'a

val debug : ('a, Format.formatter, unit, unit) format4 -> 'a
