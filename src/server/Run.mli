
(* This file is free software, part of frog-utils. See file "license" for more details. *)
open Frog

type env = (string * string) array

val run_proc :
  timeout:int ->
  Lwt_process.command ->
  Event.raw_result Lwt.t

val run_prover :
  ?env:env ->
  timeout:int ->
  memory:int ->
  prover:Prover.t ->
  pb:Problem.t ->
  unit ->
  Event.prover Event.result Lwt.t
(** Runs the prover in a sub-process, and returns a the result *)

val maki_raw_res : Event.raw_result Maki.Value.ops
val maki_result : Event.prover Event.result Maki.Value.ops

