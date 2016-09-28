
(* This file is free software, part of frog-utils. See file "license" for more details. *)

type t =
  | Sat
  | Unsat
  | Unknown
  | Timeout
  | Error
  [@@deriving yojson]

val compare: t -> t -> [`Same | `LeftBetter | `RightBetter | `Mismatch]
(** [compare a b] compares results [a] and [b] (assuming they are results
    of two distinct provers on the same problem), and returns:

    {ul
      {- `Same if results coincide}
      {- `Mismatch if they are not compatible (error)}
      {- `LeftBetter if [b = Unknown] and [a = Sat] or [a = Unsat]}
      {- `RightBetter if [a = Unknown] and [b = Sat] or [b = Unsat]}
    }
*)

val to_string : t -> string
val of_string : string -> t

val print : Format.formatter -> t -> unit
val maki : t Maki.Value.ops
val to_html : t -> Html.t
