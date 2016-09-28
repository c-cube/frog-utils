
(* This file is free software, part of frog-utils. See file "license" for more details. *)

type t = Problem.t list

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
