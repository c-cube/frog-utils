
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Result
open Frog

module E = Misc.Err

(** {2 Maki} *)

let maki_raw_res =
  Maki_yojson.make "raw_res"
    ~to_yojson:Event.raw_result_to_yojson
    ~of_yojson:Event.raw_result_of_yojson

let (maki_result : Event.prover Event.result Maki.Codec.t) =
  let of_yojson x = Event.result_of_yojson Event.prover_of_yojson x in
  Maki_yojson.make "result"
    ~to_yojson:(Event.result_to_yojson Event.prover_to_yojson)
    ~of_yojson

let maki_storage =
  Maki_yojson.make "storage_dirs"
    ~to_yojson:Storage.to_yojson ~of_yojson:Storage.of_yojson

let maki_snapshot_meta =
  Maki_yojson.make "snapshot_meta"
    ~to_yojson:Event.Meta.to_yojson ~of_yojson:Event.Meta.of_yojson

(** {2 Main} *)

(* Start processes *)
type env = (string * string) array

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
           Lwt.return { Event.
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
           Lwt.return { Event.
             stdout = ""; stderr; errcode;
             rtime; utime = 0.; stime = 0.; }
       in
       res
    )

let run_prover ?env ~timeout ~memory ~prover ~pb () =
  let file = pb.Problem.name in
  let cmd = mk_cmd ?env ~timeout ~memory ~prover ~file () in
  let%lwt raw = run_proc ~timeout cmd in
  Lwt.return { Event.
    program = prover;
    problem = pb; raw; }

let exec_prover ?(env=[||]) ~timeout ~memory ~prover ~file () =
  let p, args = mk_cmd ~env ~timeout ~memory ~prover ~file () in
  let env = Array.map (fun (a,b) -> a ^"="^ b) env in
  Unix.execvpe p args env

module TPTP = struct
  let exec_prover ?tptp ~config ~timeout ~memory ~prover ~file () =
    let env = match tptp with
      | Some f -> [| "TPTP", f |]
      | None ->
        begin match Config.(get config (string "TPTP")) with
          | Error _ -> [| |]
          | Ok f -> [| "TPTP", f |]
        end
    in
    exec_prover ~env ~timeout ~memory ~prover ~file ()
end
