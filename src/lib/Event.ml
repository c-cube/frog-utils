
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

type 'a printer = Format.formatter -> 'a -> unit

type t =
  | Prover_run of Run.prover Run.result
  | Checker_run of Run.checker Run.result
[@@deriving yojson]

let to_string r = to_yojson r |> Yojson.Safe.to_string
let pp out r = Format.pp_print_string out (to_string r)

type events = t list [@@deriving yojson]

let pp_events out l =
  Format.fprintf out "[@[%a@]]" (Misc.Fmt.pp_list ~start:"" ~stop:"" pp) l
