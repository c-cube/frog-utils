
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Frog

type t = Problem.t
type path = string

val find_expect :
  expect:Test.Config.expect ->
  path ->
  Res.t Misc.Err.t Lwt.t
(** FInd the expected result for this given problem *)

val make :
  find_expect:(path -> Res.t Misc.Err.t Lwt.t) ->
  path ->
  Problem.t Misc.Err.t Lwt.t
(** [make ~find_expect file] tries to find the expected result of [file], and
    makes a problem if it finds the result
    @param find_expect the function to obtain the actual expected result *)

val of_dir :
  filter:(string -> bool) ->
  path ->
  path list Lwt.t
(** Traverse the directory and returns all files that match the given filter *)

module Set : sig
  type t = Problem.problem_set

  val size : t -> int

  val make:
    find_expect:(path -> Res.t Misc.Err.t Lwt.t) ->
    string list ->
    t Misc.Err.t Lwt.t
    (** Build a set of problems out of file names *)

  val of_dir :
    expect:Test.Config.expect ->
    filter:(string -> bool) ->
    string ->
    t Misc.Err.t Lwt.t
    (** Traverse the directory and makes a problem set out of every
        file it contains.
        @param filter if present, only files that satisfy the predicate are used *)

  val print: Format.formatter -> t -> unit
end
