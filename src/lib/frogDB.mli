
(* This file is free software, part of frog-utils. See file "license" for more details. *)

type t
type prover = Int64.t
type problem = Int64.t

module S : Sqlexpr_sqlite.S
  with type db = t
   and type 'a result = 'a

val open_db : string -> t

val prover_id : t -> FrogProver.t -> prover option
val problem_id : t -> FrogProblem.t -> problem option

val find_prover : t -> prover -> FrogProver.t
val find_problem : t -> problem -> FrogProblem.t

val add_prover : t -> FrogProver.t -> unit
val add_problem : t -> FrogProblem.t -> unit

val add_result : t -> FrogMap.raw_result -> unit
val add_list : t -> FrogProver.t -> FrogMap.raw_result list -> unit

