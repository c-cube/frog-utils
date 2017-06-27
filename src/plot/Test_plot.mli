
open Frog

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
