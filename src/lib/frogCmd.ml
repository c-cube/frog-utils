
(* This file is free software, part of frog-utils. See file "license" for more details. *)

module Res = FrogRes
module Conf = FrogConfig
module Prover = FrogProver

type env = (string * string) array

let extract_res_ ~prover stdout stderr errcode =
  (* find if [re: re option] is present in [stdout] *)
  let find_opt_ re = match re with
    | None -> false
    | Some re ->
      Re.execp (Re_posix.compile_pat re) stdout ||
      Re.execp (Re_posix.compile_pat re) stderr
  in
  if errcode = 0 && find_opt_ prover.Prover.sat then Res.Sat
  else if errcode = 0 && find_opt_ prover.Prover.unsat then Res.Unsat
  else if (find_opt_ prover.Prover.timeout || find_opt_ prover.Prover.unknown)
  then Res.Unknown
  else Res.Error

(* command ready to run in a shell *)
let make_command ?(env=[||]) p ~timeout ~memory ~file =
  let buf = Buffer.create 32 in
  let add_str s =
    Buffer.add_substitute buf
      (function
        | "memory" -> string_of_int memory
        | "timeout" | "time" -> string_of_int timeout
        | "file" -> file
        | "binary" -> p.Prover.binary
        | _ -> raise Not_found)
      s
  in
  (* XXX: seems to make zombie processes?
  add_str "ulimit -t \\$(( 1 + $time)) -v \\$(( 1000000 * $memory )); ";
  *)
  Array.iter
    (fun (key,value) -> add_str (key ^ "=" ^ value ^ " "))
    env;
  add_str p.Prover.cmd;
  Buffer.contents buf

let run_cmd ?env ~timeout ~memory ~prover ~file () =
  Lwt_log.ign_debug_f "timeout: %d, memory: %d" timeout memory;
  let cmd = make_command ?env prover ~timeout ~memory ~file in
  let cmd = ["/bin/sh"; "-c"; cmd] in
  "/bin/sh", Array.of_list cmd

let run_exec ?env ~timeout ~memory ~prover ~file () =
  let cmd, args = run_cmd ?env ~timeout ~memory ~prover ~file () in
  Unix.execv cmd args

let run_proc ?env ~timeout ~memory ~prover ~pb () =
  let file = pb.FrogProblem.name in
  let cmd = run_cmd ?env ~timeout ~memory ~prover ~file () in
  let start = Unix.gettimeofday () in
  (* slightly extended timeout *)
  let res =
    Lwt_process.with_process_full ~timeout:(float timeout +. 1.) cmd
    (fun p ->
      let res =
        let res_errcode =
          Lwt.map
            (function
              | Unix.WEXITED e
              | Unix.WSIGNALED e
              | Unix.WSTOPPED e -> e)
            p#status
        in
        try%lwt
          let%lwt () = Lwt_io.close p#stdin in
          let out = Lwt_io.read p#stdout in
          let err = Lwt_io.read p#stderr in
          let%lwt errcode = res_errcode in
          Lwt_log.ign_debug_f "errcode: %d\n" errcode;
          let rtime = Unix.gettimeofday () -. start in
          (* now finish reading *)
          Lwt_log.ign_debug_f "reading...\n";
          let%lwt stdout = out in
          let%lwt stderr = err in
          Lwt_log.ign_debug_f "closing...\n";
          let%lwt _ = p#close in
          Lwt_log.ign_debug_f "done closing & reading, return\n";
          let%lwt rusage = p#rusage in
          let utime = rusage.Lwt_unix.ru_utime in
          let stime = rusage.Lwt_unix.ru_stime in
          let res = extract_res_ ~prover stdout stderr errcode in
          Lwt.return {
            FrogMap.problem = pb; prover; res;
            stdout; stderr; errcode;
            rtime; utime; stime; }
        with e ->
          let%lwt errcode = res_errcode in
          Lwt_log.ign_debug_f ~exn:e "error while running %s"
            ([%show: (string * string array)] cmd);
          Lwt.return {
            FrogMap.problem = pb; prover;
            res = FrogRes.Unknown;
            stdout = ""; stderr = ""; errcode;
            rtime = 0.; utime = 0.; stime = 0.; }
      in
      res
    )
  in
  let open Lwt.Infix in
  (* pick the "timeout" message for this prover, if any *)
  let str = FrogMisc.Opt.get "timeout" prover.Prover.timeout in
  let timeout_fut = Lwt_unix.sleep (float timeout +. 3.) >|=
    fun _ -> {
      FrogMap.problem = pb; prover; res = FrogRes.Unknown;
      stdout = str; stderr =  ""; errcode = 0;
      rtime = float timeout +. 3.; stime = 0.; utime = 0.; }
  in
  Lwt.pick [res; timeout_fut]

module TPTP = struct
  let get_str_ d x =
    try Some (Conf.get_string d x)
    with Not_found -> None

  let make_command ?tptp p ~timeout ~memory ~file =
    let env = match tptp with
      | None -> [||]
      | Some f -> [| "TPTP", f |]
    in
    make_command ~env p ~timeout ~memory ~file

  let run_cmd ?tptp ~timeout ~memory ~config ~prover ~file () =
    let env = match tptp with
      | Some f -> Some [| "TPTP", f |]
      | None ->
        match get_str_ config "TPTP" with
        | None -> None
        | Some f -> Some  [| "TPTP", f |]
    in
    let prover = Prover.find_config config prover in
    run_cmd ?env ~timeout ~memory ~prover ~file ()

end
