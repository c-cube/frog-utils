
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

type 'a or_error = [`Ok of 'a | `Error of string]
type 'a printer = Format.formatter -> 'a -> unit

module MStr : module type of Map.Make(String)
module Prover = FrogProver

(** {2 Result on a single problem} *)
module Res : sig
  type t =
    | Sat
    | Unsat
    | Unknown
    | Error

  val print : t printer
end

module Problem : sig
  type t = private {
    name: string;  (* filename *)
    expected: Res.t; (* result expected *)
  }

  val make: file:string -> t or_error Lwt.t
  (** [make ~file] tries to find the expected result of [file], and
      makes a problem if it finds the result *)

  val compare_res : t -> Res.t -> [`Same | `Improvement | `Mismatch]
  (** [compare_res pb res] compares the expected result of [pb] to
      the actual result [res], yielding one of:

      {ul
        {- `Same if they coincide}
        {- `Mismatch if they do not match (error)}
        {- `Improvement if unknown was expected, but sat|unsat was found}
      }
  *)

  val print : t printer
end

module ProblemSet : sig
  type t = Problem.t list

  val make: string list -> t or_error Lwt.t
  (** Build a set of problems out of file names *)

  val size : t -> int

  val of_dir : ?filter:(string -> bool) -> string -> t or_error Lwt.t
  (** Traverse the directory and makes a problem set out of every
      file it contains.
      @param filter if present, only files that satisfy the predicate are
        used *)

  val print: t printer
end

module Config : sig
  type t = {
    j: int; (* number of concurrent processes *)
    timeout: int; (* timeout for each problem *)
    prover: Prover.t;
  }

  val make: ?j:int -> ?timeout:int -> prover:Prover.t -> unit -> t
  val of_file : string -> t or_error
end

(* TODO: serialize, then make regression tests *)

module Results : sig
  type raw = (Problem.t * Res.t) MStr.t

  type stat = private {
    unsat: int;
    sat: int;
    errors: int;
    unknown: int;
  }

  type t = {
    raw: raw;
    stat: stat;
    improved: (Problem.t * Res.t) list;
    mismatch: (Problem.t * Res.t) list;
  }

  val make: raw -> t
  val of_list : (Problem.t * Res.t) list -> t

  val is_ok : t -> bool
  (** [is_ok res] is [true] iff there are no errors *)

  val print: t printer
end

val run :
  ?on_solve:(Problem.t -> Res.t -> unit Lwt.t) ->
  config:Config.t ->
  ProblemSet.t ->
  Results.t Lwt.t
(** Run the given prover on the given problem set, obtaining results
    after all the problems have been dealt with.
    @param on_solve called whenever a single problem is solved *)
