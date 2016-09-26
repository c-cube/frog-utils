
(* This file is free software, part of frog-utils. See file "license" for more details. *)

type t = {
  name: string;  (* filename *)
  expected: Res.t; (* result expected *)
} [@@deriving yojson]

val make: default_expect:Res.t option -> file:string -> unit -> t Misc.Err.t Lwt.t
(** [make ~file ()] tries to find the expected result of [file], and
    makes a problem if it finds the result
    @param default_expect if the "expect" field is not found, use this result *)

val same_name : t -> t -> bool
val compare_name : t -> t -> int
(** Compare the names of problems. *)

val compare_res : t -> Res.t -> [`Same | `Improvement | `Mismatch | `Disappoint]
(** [compare_res pb res] compares the expected result of [pb] to
    the actual result [res], yielding one of:

    {ul
      {- `Same if they coincide}
      {- `Mismatch if they do not match (error)}
      {- `Disappoint if the result is not incorrect, but less good than expected}
      {- `Improvement if unknown was expected, but sat|unsat was found}
    }
*)

val print : Format.formatter -> t -> unit
val maki : t Maki.Value.ops

val hash : t -> string

val db_init : DB.t -> unit
val db_add : DB.t -> t -> unit
val find : DB.t -> string -> t option
val find_all : DB.t -> t list

val to_html_name : t -> Web.html

val k_uri : (t -> Uri.t) Web.HMap.key
val k_add : (t -> unit) Web.HMap.key

val add_server: Web.Server.t -> unit
