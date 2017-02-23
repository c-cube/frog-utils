
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

type 'a or_error = 'a Misc.Err.t
type 'a printer = Format.formatter -> 'a -> unit
type html = Html.t
type uri = Uri.t

module MStr = Misc.StrMap

type result = Event.prover Event.result

module Raw : sig
  type t = result MStr.t

  val empty: t

  val add : result -> t -> t

  val merge : t -> t -> t

  type stat = {
    unsat: int;
    sat: int;
    errors: int;
    unknown: int;
    timeout: int;
    total_time: float; (* for sat+unsat *)
  } [@@deriving yojson]

  val stat : t -> stat

  val pp_stat : stat printer
end

(** {2 Result on a single problem} *)

module Analyze : sig
  type t = {
    raw: Raw.t;
    stat: Raw.stat;
    improved  : result list;
    ok        : result list;
    disappoint: result list;
    bad       : result list;
  } [@@deriving yojson]

  val make : Raw.t -> t

  val is_ok : t -> bool

  val num_failed : t -> int

  val of_file : file:string -> t or_error

  val to_file : file:string -> t -> unit

  val print : t printer
end

module Config : sig

  type expect =
    | Auto
    | Res of Res.t
    | Program of Prover.t
  [@@deriving yojson]

  type problem_set = {
    directory : string;
    pattern : string;
    expect : expect;
  } [@@deriving yojson]

  type t = {
    j: int; (* number of concurrent processes *)
    timeout: int; (* timeout for each problem *)
    memory: int;
    problems : problem_set list [@default []];
    provers: Prover.t list;
  } [@@deriving yojson]

  val make:
    ?j:int ->
    ?timeout:int ->
    ?memory:int ->
    ?dirs:problem_set list ->
    provers:Prover.t list ->
    unit -> t

  val update : ?j:int -> ?timeout:int -> ?memory:int -> t -> t

  val to_html : (Prover.t -> uri) -> t -> html
end

module ResultsComparison : sig
  type t = {
    appeared: (Problem.t * Res.t) list;  (* new problems *)
    disappeared: (Problem.t * Res.t) list; (* problems that disappeared *)
    improved: (Problem.t * Res.t * Res.t) list;
    regressed: (Problem.t * Res.t * Res.t) list;
    mismatch: (Problem.t * Res.t * Res.t) list;
    same: (Problem.t * Res.t * float * float) list; (* same result *)
  }

  val compare : Raw.t -> Raw.t -> t

  val print : t printer
  (** Display comparison in a readable way *)

  val print_short : t printer
  (** Display comparison in a compact way *)

  val to_html : (Problem.t -> uri) -> t -> html
end

(** {2 Top Result}

    Main result of testing: a snapshot of the work done, + the analysis
    per prover *)

type top_result = private {
  uuid: Uuidm.t lazy_t; (* unique ID *)
  timestamp: float; (* timestamp *)
  events: Event.t list;
  raw: Raw.t Prover.Map_name.t lazy_t;
  analyze: Analyze.t Prover.Map_name.t lazy_t;
}

module Top_result : sig
  type t = top_result

  val pp : t printer
  (** Full printer, including results *)

  val pp_header : t printer
  (** Print only meta-information: UUID and timestamp *)

  val merge : t -> t -> t

  val merge_l : t list -> t

  val make : ?uuid:Uuidm.t -> ?timestamp:float -> Event.t list -> t

  val snapshot : ?meta:string -> t -> Event.Snapshot.t

  val of_snapshot : Event.Snapshot.t -> t

  val to_file : file:string -> t -> unit or_error Lwt.t

  val of_file : file:string -> t or_error Lwt.t

  type comparison_result = {
    both: ResultsComparison.t Prover.Map_name.t;
    left: Analyze.t Prover.Map_name.t;
    right: Analyze.t Prover.Map_name.t;
  }

  val compare : t -> t -> comparison_result

  val pp_comparison : comparison_result printer

  type table_row = {
    tr_problem: string;
    tr_res: (string * Res.t * float) list; (* prover, result, time *)
  }

  type table = {
    t_meta: string;
    t_rows: table_row list;
    t_provers: string list;
  }

  val to_table : t -> table

  val table_to_csv : table -> Csv.t

  val to_csv : t -> Csv.t

  val to_csv_chan : out_channel -> t -> unit

  val to_csv_string : t -> string

  val to_csv_file : string -> t -> unit
  (** Write as CSV into given file *)
end

(** {2 Benchmark, within one Top Result} *)
module Bench : sig
  type per_prover = {
    stat: Raw.stat;
    sat: (string * float) list;
    unsat: (string * float) list;
  }

  type t = {
    from: top_result;
    per_prover: per_prover Prover.Map_name.t;
  }

  val make : top_result -> t

  val pp : t printer
  (** Full printer that compares provers against one another *)
end

(** {2 Compare a {!Top_result.t} with others} *)
module Summary : sig
  type individual_diff = {
    wrt: Top_result.t;
    raw_comparison: ResultsComparison.t Prover.Map_name.t; (* not empty *)
  }

  type regression_by_prover = {
    reg_prover: Prover.t;
    reg_res: (Problem.t * Res.t * Res.t) list;
  }

  (* a summary of regression *)
  type regression = {
    reg_wrt: Top_result.t;
    reg_by_prover: regression_by_prover list; (* not empty *)
  }

  type t = private {
    main: Top_result.t;
    others: individual_diff list;
    regressions: regression list;
  }

  val make : Top_result.t -> Top_result.t list -> t

  val print : t printer

  (* TODO: to_html *)
end
