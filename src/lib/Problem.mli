
(* This file is free software, part of frog-utils. See file "license" for more details. *)

type t = {
  name: string;  (* filename *)
  expected: Res.t; (* result expected *)
} [@@deriving yojson,eq]

type problem = t
type problem_set = t list [@@deriving yojson]

val make : string -> Res.t -> t
(** Make a problem. *)

val basename : t -> string
(** Returns the basename of a problem *)

val same_name : t -> t -> bool
val hash_name : t -> int
val compare_name : t -> t -> int
(** Compare the names of problems. *)

val compare_res : t -> Res.t -> [`Same | `Improvement | `Mismatch | `Disappoint | `Error]
(** [compare_res pb res] compares the expected result of [pb] to
    the actual result [res], yielding one of:

    {ul
      {- `Same if they coincide}
      {- `Mismatch if they do not match in an unsound way (error)}
      {- `Disappoint if the result is not incorrect, but less good than expected}
      {- `Improvement if unknown was expected, but sat|unsat was found}
      {- `Error if the actual result is an error but not the expect result}
    }
*)

val print : Format.formatter -> t -> unit

val to_html_name : t -> Html.t

val uri_of_problem : t -> Uri.t

(** {2 Proper table for storing problems} *)
module Tbl : sig
  type t

  val empty : t

  val add : problem -> t -> t
  val add_l : problem list -> t -> t

  val find_by_name : string -> t -> problem option
  val to_list : t -> problem list
end
