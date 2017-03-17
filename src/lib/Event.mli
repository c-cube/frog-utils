
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a or_error = 'a Misc.Err.t

type raw_result = {
  (* Raw output *)
  errcode: int;
  stdout: string;
  stderr: string;

  (* Time used *)
  rtime : float;
  utime : float;
  stime : float;
} [@@deriving yojson,eq]

type prover  = Prover.t [@@deriving yojson]
type checker = unit [@@deriving yojson]

type +'a result = {
  program : 'a;
  problem : Problem.t;
  raw : raw_result;
}
[@@deriving yojson,eq]

val analyze_p : prover result -> Res.t

type t =
  | Prover_run of prover result
  | Checker_run of checker result
[@@deriving yojson,eq]

type event = t

val mk_prover : prover result -> t
val mk_checker : checker result -> t

val pp : t printer

(** A snapshot of a list of events, at some point, with a unique ID to
    identify it. This is designed to be stored on disk in a JSON file *)
type snapshot = private {
  uuid: Uuidm.t;
  timestamp: float;
  events: t list;
  meta: string; (* additional metadata *)
}

module Snapshot : sig
  type t = snapshot
  [@@deriving yojson,eq]

  val make : ?uuid:Uuidm.t -> ?meta:string -> ?timestamp:float -> event list -> t

  val to_file : file:string -> t -> unit or_error Lwt.t

  val of_file : file:string -> t or_error Lwt.t

  val provers : t -> Prover.Set.t

  val pp : t printer
end

type snapshot_meta = private {
  s_uuid: Uuidm.t;
  s_timestamp: float;
  s_meta: string;
  s_provers: Prover.Set.t;
  s_len: int;
}

module Meta : sig
  type t = snapshot_meta [@@deriving yojson,eq]

  val provers : t -> Prover.Set.t
  val timestamp : t -> float
  val uuid : t -> Uuidm.t
  val length : t -> int

  val pp : t printer
end

val meta : snapshot -> snapshot_meta

