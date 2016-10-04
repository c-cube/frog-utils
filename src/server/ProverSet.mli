
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Frog

module StrMap = Misc.StrMap

val of_config : Config.t -> Prover.t StrMap.t
(** Get a list of supported provers from a config file. *)

val find_config : Config.t -> string -> Prover.t
(** Parse prover description from config file, and check it is listed
    in the "provers" list
    @raise Failure if prover not found *)
