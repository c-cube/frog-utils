
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

type 'a printer = Format.formatter -> 'a -> unit

type t =
  | Prover_run of Run.prover Run.result
  | Checker_run of Run.checker Run.result
[@@deriving yojson]

val pp : t printer

type events = t list [@@deriving yojson]

val pp_events : events printer
