
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
  | (Unknown | Timeout), (Sat | Unsat) -> `RightBetter
  | (Sat | Unsat), (Unknown | Timeout) -> `LeftBetter
  | (Unsat | Error), Sat
  | (Sat | Error), Unsat
  | Error, (Unknown | Timeout)
  | (Sat | Unknown | Timeout | Unsat), Error ->
    `Mismatch

let maki : t Maki.Value.ops =
  let of_yojson x = FrogMisc.Err.to_exn (of_yojson x) in
  Maki_yojson.make ~to_yojson ~of_yojson "result"

let to_html s =
  let module H = FrogWeb.Html in
  let color = match s with
    | Unsat | Sat -> "darkgreen"
    | Timeout
    | Unknown -> "orange"
    | Error -> "red"
  in
  H.string (to_string s)
  |> H.span ~cls:"result" ~attrs:["style", "color:"^color]


