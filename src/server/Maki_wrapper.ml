
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Maki Wrappers} *)

open Frog

module E = Misc.Err

let res : Res.t Maki.Codec.t =
  let of_yojson x = Res.of_yojson x in
  Maki_yojson.make ~to_yojson:Res.to_yojson ~of_yojson "result"

let problem : Problem.t Maki.Codec.t =
  Maki_yojson.make "problem"
    ~to_yojson:Problem.to_yojson
    ~of_yojson:Problem.of_yojson

let problem_set : Problem.problem_set Maki.Codec.t =
  Maki_yojson.make "problem-set"
    ~to_yojson:Problem.problem_set_to_yojson
    ~of_yojson:Problem.problem_set_of_yojson

let prover : Prover.t Maki.Codec.t =
  Maki_yojson.make "prover"
    ~to_yojson:Prover.to_yojson
    ~of_yojson:Prover.of_yojson

let test_config : Test.Config.t Maki.Codec.t =
  let open Test.Config in
  Maki_yojson.make ~of_yojson ~to_yojson "config_json"

let test_analyze : Test.Analyze.t Maki.Codec.t =
  let open Test.Analyze in
  Maki_yojson.make "results"
    ~to_yojson
    ~of_yojson
