
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
    [@@deriving yojson]

  val compare: t -> t -> [`Same | `LeftBetter | `RightBetter | `Mismatch]
  (** [compare a b] compares results [a] and [b] (assuming they are results
      of two distinct provers on the same problem), and returns:

      {ul
        {- `Same if results coincide}
        {- `Mismatch if they are not compatible (error)}
        {- `LeftBetter if [b = Unknown] and [a = Sat] or [a = Unsat]}
        {- `RightBetter if [a = Unknown] and [b = Sat] or [b = Unsat]}
      }
  *)

  val print : t printer
  val maki : t Maki.Value.ops
end

module Problem : sig
  type t = private {
    name: string;  (* filename *)
    expected: Res.t; (* result expected *)
  } [@@deriving yojson]

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
  val maki : t Maki.Value.ops
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
  val maki : t Maki.Value.ops
end

module Config : sig
  type t = {
    j: int; (* number of concurrent processes *)
    timeout: int; (* timeout for each problem *)
    memory: int;
    problem_pat: string; (* regex for problems *)
    prover: Prover.t;
  } [@@deriving yojson]

  val make: ?j:int -> ?timeout:int -> ?memory:int -> pat:string -> prover:Prover.t -> unit -> t
  val of_file : string -> t or_error
  val maki : t Maki.Value.ops
end

(* TODO: serialize, then make regression tests *)

module Results : sig
  type raw = (Problem.t * Res.t) MStr.t
  [@@deriving yojson]

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
  } [@@deriving yojson]

  val make: raw -> t

  val of_list : (Problem.t * Res.t) list -> t

  val of_file : file:string -> t or_error
  (** Parse JSON file *)

  val to_file : t -> file:string -> unit
  (** Write JSON file *)

  val num_failed : t -> int
  (** [= 0] iff [is_ok] *)

  val is_ok : t -> bool
  (** [is_ok res] is [true] iff there are no errors *)

  val print: t printer
  val maki : t Maki.Value.ops
end

module ResultsComparison : sig
  type t = {
    appeared: (Problem.t * Res.t) list;  (* new problems *)
    disappeared: (Problem.t * Res.t) list; (* problems that disappeared *)
    improved: (Problem.t * Res.t * Res.t) list;
    regressed: (Problem.t * Res.t * Res.t) list;
    mismatch: (Problem.t * Res.t * Res.t) list;
    same: (Problem.t * Res.t) list; (* same result *)
  }

  val compare : Results.raw -> Results.raw -> t

  val print : t printer
  (** Display comparison in a readable way *)
end

val run :
  ?on_solve:(Problem.t -> Res.t -> unit Lwt.t) ->
  ?caching:bool ->
  ?j:int ->
  ?timeout:int ->
  ?memory:int ->
  config:Config.t ->
  ProblemSet.t ->
  Results.t Lwt.t
(** Run the given prover on the given problem set, obtaining results
    after all the problems have been dealt with.
    @param caching if true, use Maki for caching results (default true)
    @param on_solve called whenever a single problem is solved *)

