
(* This file is free software, part of frog-utils. See file "license" for more details. *)

[@@@warning "-39"]
type t =
  | Sat
  | Unsat
  | Unknown
  | Timeout
  | Error
  [@@deriving yojson]
[@@@warning "+39"]

let to_string = function
  | Sat -> "sat"
  | Unsat -> "unsat"
  | Unknown -> "unknown"
  | Timeout -> "timeout"
  | Error -> "error"

let of_string = function
  | "sat" -> Sat
  | "unsat" -> Unsat
  | "error" -> Error
  | "timeout" -> Timeout
  | "unknown" -> Unknown
  | s -> failwith ("unknown result: " ^ s)

let print out s = Format.pp_print_string out (to_string s)

let compare a b = match a, b with
  | Unsat, Unsat
  | Sat, Sat
  | Unknown, Unknown
  | Timeout, Timeout
  | Error, Error -> `Same
  | Unknown, Timeout -> `LeftBetter
  | Timeout, Unknown -> `RightBetter
  | (Unknown | Timeout | Error), (Sat | Unsat) -> `RightBetter
  | (Sat | Unsat), (Unknown | Timeout | Error) -> `LeftBetter
  | Error, (Unknown | Timeout) -> `RightBetter
  | (Unknown | Timeout), Error -> `LeftBetter
  | Unsat, Sat
  | Sat, Unsat ->
    `Mismatch

let to_html s =
  let module H = Html in
  let color = match s with
    | Unsat | Sat -> "darkgreen"
    | Timeout
    | Unknown -> "orange"
    | Error -> "red"
  in
  H.pcdata (to_string s)
  |> Misc.List.return
  |> H.span ~a:[H.a_class ["result"]; H.a_style ("color:"^color)]


