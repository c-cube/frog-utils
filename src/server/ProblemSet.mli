
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Frog

type t = Problem.problem_set

val make_pb :
  default_expect:Res.t option ->
  file:string -> unit -> Problem.t Misc.Err.t Lwt.t
(** [make ~file ()] tries to find the expected result of [file], and
    makes a problem if it finds the result
    @param default_expect if the "expect" field is not found, use this result *)

val make: default_expect:Res.t option -> string list -> t Misc.Err.t Lwt.t
(** Build a set of problems out of file names *)

val size : t -> int

val of_dir :
  default_expect:Res.t option ->
  ?filter:(string -> bool) ->
  string ->
  t Misc.Err.t Lwt.t
(** Traverse the directory and makes a problem set out of every
    file it contains.
    @param filter if present, only files that satisfy the predicate are
      used *)

val print: Format.formatter -> t -> unit
