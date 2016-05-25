
(* This file is free software, part of frog-utils. See file "license" for more details. *)

module MStr : Map.S with type key = string

type raw_result = {
  problem: FrogProblem.t;
  res: FrogRes.t;
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
  improved: raw_result list;
  ok: raw_result list;
  disappoint: raw_result list;
  bad: raw_result list;
}

val make: raw -> t

val add_raw : raw -> raw_result -> raw

val of_list : raw_result list -> t

val of_file : file:string -> t FrogMisc.Err.t
(** Parse JSON file *)

val to_file : t -> file:string -> unit
(** Write JSON file *)

val num_failed : t -> int
(** [= 0] iff [is_ok] *)

val is_ok : t -> bool
(** [is_ok res] is [true] iff there are no errors *)

val print: Format.formatter -> t -> unit
val maki : t Maki.Value.ops
val maki_raw_res : raw_result Maki.Value.ops

val to_html_raw : (FrogProblem.t -> FrogWeb.uri) -> (raw_result -> FrogWeb.uri) -> raw -> FrogWeb.html
val to_html : (FrogProblem.t -> FrogWeb.uri) -> (raw_result -> FrogWeb.uri) -> t -> FrogWeb.html

val k_add : (raw_result -> unit) FrogWeb.HMap.key
val k_set : (t -> unit) FrogWeb.HMap.key
val add_server : FrogWeb.Server.t -> unit

