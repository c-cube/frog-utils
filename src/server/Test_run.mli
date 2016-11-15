
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Tests} *)

open Frog

type 'a or_error = 'a Misc.Err.t

val config_of_file : string -> Test.Config.t Misc.Err.t

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

val find_results : ?storage:Storage.t -> string -> Test.Top_result.t or_error Lwt.t
(** Find results in file or storage (try both) *)

val all_results : Storage.t -> Test.Top_result.t list or_error Lwt.t
(** All test results *)

val last_result : Storage.t -> Test.Top_result.t or_error Lwt.t
(** Most recent test result *)

val find_or_last : ?storage:Storage.t -> string option -> Test.Top_result.t or_error Lwt.t
(** Try to find the file [f], if [Some f] is given, or
    if [None] is passed then return {!last_result} *)
