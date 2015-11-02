
type graph_config
(** Abstract type for graph configuration *)

type drawer = private graph_config -> Archimedes.Viewport.t -> unit
(** The type of functions that draw on a graph *)

val list : drawer list -> drawer
(** Combine drawers *)

val float_list : ?sort:bool -> float list * string -> drawer
(** Prints the given list of floats, possibly sorted if [~sort] is set to [true]. *)

val float_sum :
  ?filter:int -> ?count:int -> ?sort:bool ->
  float list * string -> drawer
(** [float_sum (l, name)] draw the cumulative sum of the floats in [l], and
    prints [name] at the end of the line.
    If [~sort] is [true], then [l] is sorted before drawing.
    [~filter:k] : prints only one every [k] points.
    [~count:n] : prints the last [n] points regardless of [~filter]. *)

val draw_on_graph : graph_config -> fmt:string -> file:string -> drawer -> unit
(** [draw_on_graph config format filename f] outputs a graph to the given format
    in the given filename. Axes and labels are drawn according to the configuration provided.
    The function [f] is used to draw user-defined content on the graph *)

val graph_section : string
(** Manpage section of the graph configuration options *)

val graph_args : graph_config Cmdliner.Term.t
(** Cmdliner term that evaluates to a graph confguration. *)

