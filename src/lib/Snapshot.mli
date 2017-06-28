
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Snapshots, i.e collections of events} *)

(** A snapshot if a collection of events. As the collection is meant
    to be quite sizable, we avoid having the list of events in memory
    (as it would most probably exceed available RAM). *)
type t = private {
  uuid: Uuidm.t;
  timestamp: float;
  meta: string; (* additional metadata *)
}
[@@deriving yojson, eq]

(** Create a snapshot *)
val make :
  ?uuid:Uuidm.t -> ?meta:string ->
  ?timestamp:float -> unit -> t

(** Print a snapshot *)
val pp : Format.formatter -> t -> unit

(** Meta information about snapshots *)
module Meta : sig
  type t = private {
    s_uuid: Uuidm.t;
    s_timestamp: float;
    s_meta: string;
    s_provers: Prover.Set.t;
    s_len: int;
  }
  [@@deriving yojson, eq]

  val provers : t -> Prover.Set.t
  val timestamp : t -> float
  val uuid : t -> Uuidm.t
  val length : t -> int

  val pp : Format.formatter -> t -> unit
end

