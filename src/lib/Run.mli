
(* This file is free software, part of frog-utils. See file "license" for more details. *)

type env = (string * string) array

type raw_result = {
  (* Raw output *)
  errcode: int;
  stdout: string;
  stderr: string;

  (* Time used *)
  rtime : float;
  utime : float;
  stime : float;
} [@@deriving yojson]

type prover  = [ `Prover of Prover.t ]  [@@deriving yojson]
type checker = [ `Checker of unit ]         [@@deriving yojson]
type program = [ prover | checker ]         [@@deriving yojson]

type +'a result = {
  program : 'a;
  problem : Problem.t;
  raw : raw_result;
} constraint 'a = [< program ]
    [@@deriving yojson]

val run_proc :
  timeout:int ->
  Lwt_process.command ->
  raw_result Lwt.t

val run_prover :
  ?env:env ->
  timeout:int ->
  memory:int ->
  prover:Prover.t ->
  pb:Problem.t ->
  unit ->
  prover result Lwt.t
(** Runs the prover in a sub-process, and returns a the result *)

val analyze_p : prover result -> Res.t

val maki_raw_res : raw_result Maki.Value.ops
val maki_result : program result Maki.Value.ops

