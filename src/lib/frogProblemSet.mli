
(* This file is free software, part of frog-utils. See file "license" for more details. *)

type t = FrogProblem.t list

val make: string list -> t FrogMisc.Err.t Lwt.t
(** Build a set of problems out of file names *)

val size : t -> int

val of_dir : ?filter:(string -> bool) -> string -> t FrogMisc.Err.t Lwt.t
(** Traverse the directory and makes a problem set out of every
    file it contains.
    @param filter if present, only files that satisfy the predicate are
      used *)

val print: Format.formatter -> t -> unit
val maki : t Maki.Value.ops
val to_html : (FrogProblem.t -> FrogWeb.uri) -> t -> FrogWeb.html
