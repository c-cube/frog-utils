
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Tests} *)

open Frog

val run :
  ?on_solve:(Test.result -> unit Lwt.t) ->
  ?on_done:(Test.top_result -> unit Lwt.t) ->
  ?caching:bool ->
  ?j:int ->
  ?timeout:int ->
  ?memory:int ->
  ?provers:string list ->
  config:Test.Config.t ->
  ProblemSet.t ->
  Test.top_result Lwt.t
(** Run the given prover(s) on the given problem set, obtaining results
    after all the problems have been dealt with.
    @param caching if true, use Maki for caching results (default true)
    @param on_solve called whenever a single problem is solved *)

