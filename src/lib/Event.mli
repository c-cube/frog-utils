
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a or_error = 'a Misc.Err.t

type t =
  | Prover_run of Run.prover Run.result
  | Checker_run of Run.checker Run.result
[@@deriving yojson]

type event = t

val mk_prover : Run.prover Run.result -> t
val mk_checker : Run.checker Run.result -> t

val pp : t printer

(** A snapshot of a list of events, at some point, with a unique ID to
    identify it. This is designed to be stored on disk in a JSON file *)
type snapshot = private {
  uuid: Uuidm.t;
  timestamp: float;
  events: t list;
}

module Snapshot : sig
  type t = snapshot
  [@@deriving yojson]

  val make : ?timestamp:float -> event list -> t

  val to_file : file:string -> t -> unit or_error Lwt.t

  val of_file : file:string -> t or_error Lwt.t

  val pp : t printer
end
