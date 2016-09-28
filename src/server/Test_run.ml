
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Tests} *)

open Frog
open Lwt.Infix

module C = Test.Config
module T = Test

(* run one particular test *)
let run_pb_ ~config prover pb =
  Lwt_log.ign_debug_f "running %-15s/%-30s..."
    (Filename.basename prover.Prover.binary) pb.Problem.name;
  (* spawn process *)
  let%lwt result = Run.run_prover
      ~timeout:config.C.timeout
      ~memory:config.C.memory
      ~prover ~pb ()
  in
  Lwt_log.ign_debug_f "output for %s/%s: `%s`, `%s`, errcode %d"
    prover.Prover.binary pb.Problem.name
    result.Event.raw.Event.stdout
    result.Event.raw.Event.stderr
    result.Event.raw.Event.errcode;
  Lwt.return result

let run_pb ?(caching=true) ?limit ~config prover pb =
  let module V = Maki.Value in
  Maki.call_exn
    ?limit
    ~bypass:(not caching)
    ~lifetime:(`KeepFor Maki.Time.(days 2))
    ~deps:[V.pack V.int config.C.timeout;
           V.pack V.int config.C.memory;
           V.pack Maki_wrapper.prover prover;
           V.pack Maki_wrapper.problem pb]
    ~op:Run.maki_result
    ~name:"frogtest.run_pb"
    (fun () -> run_pb_ ~config prover pb)

let nop_ _ = Lwt.return_unit

let run ?(on_solve = nop_) ?(on_done = nop_)
    ?(caching=true) ?j ?timeout ?memory ?provers ~config set
  =
  let config = C.update ?j ?timeout ?memory config in
  let limit = Maki.Limit.create config.C.j in
  let provers = match provers with
    | None -> config.C.provers
    | Some l ->
      List.filter
        (fun p -> List.mem (Prover.name p) l)
        config.C.provers
  in
  let%lwt res =
    Lwt_list.map_p
      (fun prover ->
         let%lwt l =
           Lwt_list.map_p
             (fun pb ->
                let%lwt result = run_pb ~caching ~limit ~config prover pb in
                let%lwt () = on_solve result in (* callback *)
                Lwt.return result)
             set
         in
         Lwt.return l)
      provers
    >|= List.flatten
  in
  let r = T.Top_result.make (List.map Event.mk_prover res) in
  let%lwt () = on_done r in
  Lwt.return r

