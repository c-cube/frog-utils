
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Result

module W = Web
module H = W.Html
module R = W.Record
module E = Misc.Err

let fpf = Format.fprintf

type raw_result = {
  (* Raw output *)
  errcode: int;
  stdout: string;
  stderr: string;

  (* Time used *)
  rtime : float;
  utime : float;
  stime : float;
} [@@deriving yojson]

type prover  = Prover.t [@@deriving yojson]
type checker = unit [@@deriving yojson]

type +'a result = {
  program : 'a;
  problem : Problem.t;
  raw : raw_result;
} [@@deriving yojson]

let hash_prog t =
  match t.program with
  | `Prover prover -> Prover.hash prover
  | `Checker checker -> "TODO"

let maki_raw_res =
  let of_yojson x = E.to_exn (raw_result_of_yojson x) in
  Maki_yojson.make "raw_res"
    ~to_yojson:raw_result_to_yojson
    ~of_yojson

let (maki_result : prover result Maki.Value.ops) =
  let of_yojson x = E.to_exn (result_of_yojson prover_of_yojson x) in
  Maki_yojson.make "result"
    ~to_yojson:(result_to_yojson prover_to_yojson)
    ~of_yojson

(* Start processes *)
type env = (string * string) array

let analyze_p t =
  let prover = t.program in
  (* find if [re: re option] is present in [stdout] *)
  let find_opt_ re = match re with
    | None -> false
    | Some re ->
      Re.execp (Re_posix.compile_pat re) t.raw.stdout ||
      Re.execp (Re_posix.compile_pat re) t.raw.stderr
  in
  if t.raw.errcode = 0 then
    if find_opt_ prover.Prover.sat then Res.Sat
    else if find_opt_ prover.Prover.unsat then Res.Unsat
    else if find_opt_ prover.Prover.timeout then Res.Timeout
    else Res.Unknown
  else if find_opt_ prover.Prover.timeout then Res.Timeout
  else if find_opt_ prover.Prover.unknown then Res.Unknown
  else Res.Error

let mk_cmd ?env ~timeout ~memory ~prover ~file () =
  Lwt_log.ign_debug_f "timeout: %d, memory: %d" timeout memory;
  let cmd = Prover.make_command ?env prover ~timeout ~memory ~file in
  let cmd = ["/bin/sh"; "-c"; cmd] in
  "/bin/sh", Array.of_list cmd

let run_proc ~timeout cmd =
  let start = Unix.gettimeofday () in
  (* slightly extended timeout *)
  Lwt_process.with_process_full cmd
    (fun p ->
       let killed = ref false in
       (* Setup alarm to enforce timeout *)
       let pid = p#pid in
       let al = Lwt_timeout.create (timeout + 1)
           (fun () -> killed := true; Unix.kill pid Sys.sigkill) in
       Lwt_timeout.start al;
       (* Convenience function to get error code *)
       let res =
         let res_errcode =
           Lwt.map
             (function
               | Unix.WEXITED e
               | Unix.WSIGNALED e
               | Unix.WSTOPPED e -> e)
             p#status
         in
         (* Wait for the process to end, then get all output *)
         try%lwt
           let%lwt () = Lwt_io.close p#stdin in
           let out = Lwt_io.read p#stdout in
           let err = Lwt_io.read p#stderr in
           let%lwt code = res_errcode in
           let errcode = if !killed then 0 else code in
           Lwt_log.ign_debug_f "errcode: %d\n" errcode;
           Lwt_timeout.stop al;
           (* Compute time used by the prover *)
           let rtime = Unix.gettimeofday () -. start in
           let%lwt rusage = p#rusage in
           let utime = rusage.Lwt_unix.ru_utime in
           let stime = rusage.Lwt_unix.ru_stime in
           (* now finish reading *)
           Lwt_log.ign_debug_f "reading...\n";
           let%lwt stdout = out in
           let%lwt stderr = err in
           Lwt_log.ign_debug_f "closing...\n";
           let%lwt _ = p#close in
           Lwt_log.ign_debug_f "done closing & reading, return\n";
           Lwt.return {
             stdout; stderr; errcode;
             rtime; utime; stime; }
         with e ->
           Lwt_timeout.stop al;
           let%lwt code = res_errcode in
           let errcode = if !killed then 0 else code in
           let rtime = Unix.gettimeofday () -. start in
           let stderr = Printexc.to_string e in
           Lwt_log.ign_debug_f ~exn:e "error while running %s"
             ([%show: (string * string array)] cmd);
           Lwt.return {
             stdout = ""; stderr; errcode;
             rtime; utime = 0.; stime = 0.; }
       in
       res
    )

let run_prover ?env ~timeout ~memory ~prover ~pb () =
  let file = pb.Problem.name in
  let cmd = mk_cmd ?env ~timeout ~memory ~prover ~file () in
  let%lwt raw = run_proc ~timeout cmd in
  Lwt.return {
    program = prover;
    problem = pb; raw; }
