
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

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

val program : 'a result -> 'a
val problem : _ result -> Problem.t
val raw : _ result -> raw_result

val analyze_p : prover result -> Res.t

type t =
  | Prover_run of prover result
  | Checker_run of checker result
[@@deriving yojson,eq]

type event = t

val mk_prover : prover result -> t
val mk_checker : checker result -> t

val pp : Format.formatter -> t -> unit

