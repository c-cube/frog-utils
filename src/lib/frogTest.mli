
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

type 'a or_error = 'a FrogMisc.Err.t
type 'a printer = Format.formatter -> 'a -> unit
type html = FrogWeb.html
type uri = FrogWeb.uri

module Prover = FrogProver

module Res = FrogRes
module Problem = FrogProblem
module ProblemSet = FrogProblemSet

module MStr : Map.S with type key = String.t

type result = FrogRun.prover FrogRun.result

(** {2 Result on a single problem} *)

module Analyze : sig
  type raw = result MStr.t

  type stat = {
    unsat: int;
    sat: int;
    errors: int;
    unknown: int;
    timeout: int;
  } [@@deriving yojson]

  type t = {
    raw: raw;
    stat: stat;
    improved  : result list;
    ok        : result list;
    disappoint: result list;
    bad       : result list;
  } [@@deriving yojson]

  val is_ok : t -> bool

  val num_failed : t -> int

  val of_file : file:string -> t or_error

  val to_file : file:string -> t -> unit

  val print : Format.formatter -> t -> unit

  val to_junit : t -> Junit.Testsuite.t
  (** Converts the results into a junit testsuite *)

  val junit_to_file : Junit.Testsuite.t list -> string -> unit
  (** [to_junit_file j file] writes the testsuite [j] into given file *)
end

module Config : sig
  type t = {
    j: int; (* number of concurrent processes *)
    timeout: int; (* timeout for each problem *)
    memory: int;
    default_dirs: string list;
    default_expect: Res.t option; (* default status for problems *)
    problem_pat: string; (* regex for problems *)
    provers: Prover.t list;
  } [@@deriving yojson]

  val make:
    ?j:int ->
    ?timeout:int ->
    ?memory:int ->
    ?dir:string list ->
    ?default_expect:Res.t ->
    pat:string ->
    provers:Prover.t list ->
    unit -> t

  val of_file : string -> t or_error

  val maki : t Maki.Value.ops

  val to_html : (Prover.t -> uri) -> t -> html

  val add_server : FrogWeb.Server.t -> t -> unit
end

(* TODO: serialize, then make regression tests *)

module ResultsComparison : sig
  type t = {
    appeared: (Problem.t * Res.t) list;  (* new problems *)
    disappeared: (Problem.t * Res.t) list; (* problems that disappeared *)
    improved: (Problem.t * Res.t * Res.t) list;
    regressed: (Problem.t * Res.t * Res.t) list;
    mismatch: (Problem.t * Res.t * Res.t) list;
    same: (Problem.t * Res.t) list; (* same result *)
  }

  val compare : Analyze.raw -> Analyze.raw -> t

  val print : t printer
  (** Display comparison in a readable way *)

  val to_html : (Problem.t -> uri) -> t -> html
end

val run :
  ?on_solve:(result -> unit Lwt.t) ->
  ?on_done:(Analyze.t -> unit Lwt.t) ->
  ?caching:bool ->
  ?j:int ->
  ?timeout:int ->
  ?memory:int ->
  ?db:FrogDB.t ->
  ?server:FrogWeb.Server.t ->
  ?provers:string list ->
  config:Config.t ->
  ProblemSet.t ->
  (Prover.t * Analyze.t) list Lwt.t
(** Run the given prover(s) on the given problem set, obtaining results
    after all the problems have been dealt with.
    @param caching if true, use Maki for caching results (default true)
    @param db if provided, register results in DB
    @param server if provided, register some paths to the server
    @param on_solve called whenever a single problem is solved *)

