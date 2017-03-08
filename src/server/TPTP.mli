
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Manage TPTP provers} *)

open Frog
module StrMap = Misc.StrMap

type time = {
  real : float;
  user : float;
  system : float;
}

type result = {
  res_arg     : string [@key "arg"];
  res_rtime   : float [@key "time"];
  res_utime   : (float [@default 0.]) [@key "utime"];
  res_stime   : (float [@default 0.]) [@key "stime"];
  res_errcode : int [@key "errcode"];
  res_out     : string [@key "stdout"];
  res_err     : string [@key "stderr"];
} [@@deriving yojson {strict=false},show]
(** Result of running the command on one argument *)

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

type run_params = {
  timeout : int option;
  memory : int option;
}


