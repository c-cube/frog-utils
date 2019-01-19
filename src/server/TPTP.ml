
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Manage TPTP provers} *)

open Frog
module Conf = Config
module PB = PrintBox
module StrMap = Misc.StrMap

(** {2 Type Definitions} *)

(* TODO: have a module to define times and operations on them ? *)
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

type file_summary = {
  prover : string;
  mutable num_all : int;
  mutable set_unsat : time Misc.StrMap.t;  (* pb -> time *)
  mutable set_sat : time Misc.StrMap.t; (* pb -> time *)
  mutable num_error : int;
  mutable solved_time : time; (* of successfully solved problems *)
  mutable run_time : time; (* total run time of the prover *)
}

type run_params = {
  timeout : int option;
  memory : int option;
}

let compile_re ~msg re =
  try
    Some (Re.compile (Re.no_case (Re.Posix.re re)))
  with e ->
    Printf.eprintf "could not compile regex %s: %s"
      msg (Printexc.to_string e);
    None

let execp_re_maybe maybe_re s = match maybe_re with
  | None -> false
  | Some re -> Re.execp re s

let add_time t t' = {
  real = t.real +. t'.real;
  user = t.user +. t'.user;
  system = t.system +. t'.system;
}

let time_of_res res = {
  real = res.res_rtime;
  user = res.res_utime;
  system = res.res_stime;
}
