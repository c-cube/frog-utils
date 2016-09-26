
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Manage TPTP provers} *)

module StrMap : module type of Map.Make(String)

type time = {
  real : float;
  user : float;
  system : float;
}

val time_of_res : FrogMapState.result -> time
val add_time : time -> time -> time

type file_summary = private {
  prover : string;
  mutable num_all : int;
  mutable set_unsat : time StrMap.t;  (* pb -> time *)
  mutable set_sat : time StrMap.t; (* pb -> time *)
  mutable num_error : int;
  mutable solved_time : time; (* of successfully solved problems *)
  mutable run_time : time; (* total run time of the prover *)
}

val analyse_single_file : config:FrogConfig.t -> string -> string -> file_summary

val print_file_summary : out_channel -> file_summary -> unit

type run_params = {
  timeout : int option;
  memory : int option;
}

type analyse_params = {
  get_time : time -> float;
  filter : file_summary StrMap.t -> string -> FrogMapState.result -> bool;
}

val map_summaries :
  analyse_params ->
  (StrMap.key * string * FrogProver.t * FrogMapState.job * FrogMapState.result StrMap.t) list ->
  file_summary StrMap.t

type analysis_result = {
  ar_file: string;
  ar_prover: string;
  ar_sat: int;
  ar_unsat: int;
  ar_total: int;
  ar_num_exclusive: int;
  ar_exclusive: unit StrMap.t; (* set of files solved only by this prover *)
  ar_percent_solved: float;
  ar_time: float;
  ar_avg_time: float;
  ar_errors: int;
  ar_runtime: float;
}

val analyse_multiple :
  analyse_params ->
  (StrMap.key * string * FrogProver.t * FrogMapState.job * FrogMapState.result StrMap.t) list ->
  analysis_result list

val box_of_ar : analysis_result list -> FrogPrintBox.Box.t

val print_ar_exclusive : out_channel -> analysis_result -> unit

val parse_prover_list :
  config:FrogConfig.t ->
  (string * string) list ->
  (string * string * FrogProver.t * FrogMapState.job * FrogMapState.result StrMap.t) list


