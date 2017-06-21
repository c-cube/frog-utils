
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Tests} *)

open Frog

type dir = {
  directory : string;
  pattern : string;
  expect : ProblemSet.expect;
}

type config = {
  j : int;                       (* number of concurrent processes *)
  memory : int;                  (* memory limit for each problem *)
  timeout : int;                 (* timeout for each problem *)
  provers : Prover.t list;
  problems : dir list;
}

val mk_config : Config.t -> string list -> config Misc.Err.t

val config_of_file : string -> config Misc.Err.t

val run :
  ?on_solve:(Event.prover Event.result -> unit Lwt.t) ->
  ?caching:bool -> config ->
  unit Misc.LwtErr.t
(** Run the given configuration.
    @param caching if true, use Maki for caching results (default true)
    @param on_solve called whenever a single problem is solved *)

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

  (*
  val draw : params -> Test.top_result -> Plot.drawer
  (** Make a plot out of results *)

  val draw_file : params -> Test.top_result -> unit
  (** Make a plot out of results *)
  *)

end

