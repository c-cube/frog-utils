
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Tests} *)

open Frog

type 'a or_error = 'a Misc.Err.t
type path = string

val config_of_file : string -> Test.Config.t Misc.Err.t

val config_of_config : Config.t -> string list -> Test.Config.t Misc.Err.t

val run :
  ?on_solve:(Test.result -> unit Lwt.t) ->
  ?on_done:(Test.top_result -> unit Lwt.t) ->
  ?caching:bool ->
  ?j:int ->
  ?timeout:int ->
  ?memory:int ->
  ?provers:string list ->
  expect:Test.Config.expect ->
  config:Test.Config.t ->
  path list ->
  Test.top_result or_error Lwt.t
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

module Plot_res : sig
  type data =
    | Unsat_time
    | Sat_time
    | Both_time

  type legend =
    | Prover

  type drawer =
    | Simple of bool (* should we sort the list ? *)
    | Cumul of bool * int * int (* sort, filter, count *)

  type params = {
    graph : Plot.graph_config;
    data : data;
    legend : legend;
    drawer : drawer;
    out_file : string;
    out_format : string;
  }

  val draw : params -> Test.top_result -> Plot.drawer
  (** Make a plot out of results *)

  val draw_file : params -> Test.top_result -> unit
  (** Make a plot out of results *)
end
