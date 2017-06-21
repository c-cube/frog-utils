
(* This file is free software, part of frog-utils. See file "license" for more details. *)
open Frog

type env = (string * string) array

(** {2 Main} *)

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

val exec_prover :
  ?env:env ->
  timeout:int ->
  memory:int ->
  prover:Prover.t ->
  file:string ->
  unit ->
  'a

(** {2 TPTP Provers} *)

module TPTP : sig
  val exec_prover :
    ?tptp:string ->
    config:Config.t ->
    timeout:int ->
    memory:int ->
    prover:Prover.t ->
    file:string ->
    unit ->
    'a
  (** Version of {!exec_prover} specialized to take the [tptp] env
      variable *)
end

(** {2 Maki} *)

val maki_raw_res : Event.raw_result Maki.Value.ops
val maki_result : Event.prover Event.result Maki.Value.ops
val maki_snapshot_meta : Snapshot.Meta.t Maki.Value.ops

