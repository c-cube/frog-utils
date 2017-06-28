
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Maki Wrappers} *)

open Frog

module E = Misc.Err

let string_opt =
  Maki.Value.(
    map
      (function Some s -> s | None -> "")
      (function "" -> None | s -> Some s)
      string
  )

let res : Res.t Maki.Value.ops =
  let of_yojson x = Misc.Err.to_exn (Res.of_yojson x) in
  Maki_yojson.make ~to_yojson:Res.to_yojson ~of_yojson "result"

let problem : Problem.t Maki.Value.ops =
  let module V = Maki.Value in
  let open Problem in
  V.map ~descr:"problem"
    (fun p -> p.name, p.expected)
    (fun (name, expected) -> {name;expected})
    (V.pair V.file res)

let problem_set : Problem.problem_set Maki.Value.ops = Maki.Value.set problem

let maki_version =
  Maki.Value.marshal "frogprover.version.2"

let prover : Prover.t Maki.Value.ops =
  let open Prover in
  Maki.Value.(
    map
      (fun t -> (
           (t.name, t.version),
           (t.binary, t.binary_deps,  t.cmd),
           (t.unsat, t.sat),
           (t.unknown, t.timeout, t.memory)
         ))
      (fun ((name, version), (binary, binary_deps, cmd),
        (unsat, sat), (unknown, timeout, memory)) ->
         { name; version; binary; cmd; unsat; sat;
           binary_deps; unknown; timeout; memory })
      (quad
         (pair string maki_version)
         (triple program (list program) string)
         (pair string_opt string_opt)
         (triple string_opt string_opt string_opt)
      )
  )

