
(* This file is free software, part of frog-utils. See file "license" for more details. *)

type env = (string * string) array

type raw_result = {
  (* Prover and problem *)
  prover : FrogProver.t;
  problem: FrogProblem.t;

  (* High-level result *)
  res: FrogRes.t;

  (* Raw output *)
  errcode: int;
  stdout: string;
  stderr: string;

  (* Time used *)
  rtime : float;
  utime : float;
  stime : float;
} [@@deriving yojson]

val run_proc :
  ?env:env ->
  timeout:int ->
  memory:int ->
  prover:FrogProver.t ->
  pb:FrogProblem.t ->
  unit ->
  raw_result Lwt.t
(** Runs the prover in a sub-process, and returns a raw result *)

val maki_raw_res : raw_result Maki.Value.ops

val db_init : FrogDB.t -> unit

val k_add : (raw_result -> unit) FrogWeb.HMap.key

val add_server : FrogWeb.Server.t -> unit

