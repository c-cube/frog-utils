
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

type 'a or_error = [`Ok of 'a | `Error of string]
type 'a printer = Format.formatter -> 'a -> unit
type html = FrogWeb.html
type uri = FrogWeb.uri

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
  val to_html : t -> html
end

module Problem : sig
  type t = private {
    name: string;  (* filename *)
    expected: Res.t; (* result expected *)
  } [@@deriving yojson]

  val make: file:string -> t or_error Lwt.t
  (** [make ~file] tries to find the expected result of [file], and
      makes a problem if it finds the result *)

  val compare_res : t -> Res.t -> [`Same | `Improvement | `Mismatch | `Disappoint]
  (** [compare_res pb res] compares the expected result of [pb] to
      the actual result [res], yielding one of:

      {ul
        {- `Same if they coincide}
        {- `Mismatch if they do not match (error)}
        {- `Disappoint if the result is not incorrect, but less good than expected}
        {- `Improvement if unknown was expected, but sat|unsat was found}
      }
  *)

  val print : t printer
  val maki : t Maki.Value.ops

  val to_html_full : t -> html
  val to_html_name : t -> html

  val k_uri : (t -> Uri.t) FrogWeb.HMap.key
  val k_add : (t -> unit) FrogWeb.HMap.key

  val add_server: FrogWeb.Server.t -> unit
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
  val to_html : (Problem.t -> uri) -> t -> html
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
  val to_html : (Prover.t -> uri) -> t -> html

  val add_server : FrogWeb.Server.t -> t -> unit
end

(* TODO: serialize, then make regression tests *)

module Results : sig
  type raw_result = {
    problem: Problem.t;
    res: Res.t;
    stdout: string;
    stderr: string;
    errcode: int;
  } [@@deriving yojson]

  type raw = raw_result MStr.t
  [@@deriving yojson]

  type stat = private {
    unsat: int;
    sat: int;
    errors: int;
    unknown: int;
  }

  type t = private {
    raw: raw;
    stat: stat;
    ok: raw_result list;
    disappoint: raw_result list;
    bad: raw_result list;
  }

  val make: raw -> t

  val add_raw : raw -> raw_result -> raw

  val of_list : raw_result list -> t

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
  val maki_raw_res : raw_result Maki.Value.ops

  val to_html_raw : (Problem.t -> uri) -> (raw_result -> uri) -> raw -> html
  val to_html : (Problem.t -> uri) -> (raw_result -> uri) -> t -> html

  val k_add : (raw_result -> unit) FrogWeb.HMap.key
  val add_server : FrogWeb.Server.t -> unit
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

  val to_html : (Problem.t -> uri) -> t -> html
end

val run :
  ?on_solve:(Problem.t -> Res.t -> unit Lwt.t) ->
  ?caching:bool ->
  ?j:int ->
  ?timeout:int ->
  ?memory:int ->
  ?server:FrogWeb.Server.t ->
  config:Config.t ->
  ProblemSet.t ->
  Results.t Lwt.t
(** Run the given prover on the given problem set, obtaining results
    after all the problems have been dealt with.
    @param caching if true, use Maki for caching results (default true)
    @param server if provided, register some paths to the server
    @param on_solve called whenever a single problem is solved *)

