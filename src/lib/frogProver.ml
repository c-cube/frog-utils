
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Prover} *)

module Conf = FrogConfig
module StrMap = Map.Make(String)

type env = (string * string) array

type t = {
  binary: string; (* name of the program itself *)
  cmd: string;
  (* the command line to run.
     possibly contains $binary, $file, $memory and $timeout *)
  unsat :   string option;    (* regex for "unsat" *)
  sat :     string option;      (* regex for "sat" *)
  unknown : string option;  (* regex for "unknown" *)
  timeout : string option;     (* regex for "timeout" *)
  memory :  string option;      (* regex for "out of memory" *)
} [@@deriving yojson]

let maki = Maki.Value.marshal "frog_prover"

(* command ready to run in a shell *)
let make_command ?(env=[||]) p ~timeout ~memory ~file =
  let buf = Buffer.create 32 in
  let add_str s =
    Buffer.add_substitute buf
      (function
        | "memory" -> string_of_int memory
        | "timeout" | "time" -> string_of_int timeout
        | "file" -> file
        | "binary" -> p.binary
        | _ -> raise Not_found)
      s
  in
  (* XXX: seems to make zombie processes?
  add_str "ulimit -t \\$(( 1 + $time)) -v \\$(( 1000000 * $memory )); ";
  *)
  Array.iter
    (fun (key,value) -> add_str (key ^ "=" ^ value ^ " "))
    env;
  add_str p.cmd;
  Buffer.contents buf

let get_str_ d x =
  try Some (Conf.get_string d x)
  with Not_found -> None

(* recover description of prover from config file *)
let build_from_config config name =
  let d =
    try Conf.get_table config name
    with Not_found ->
      failwith ("could not find prover " ^ name ^ " in config")
  in
  let cmd = Conf.get_string d "cmd" |> String.trim in
  let binary =
    try Conf.get_string d "binary"
    with Not_found ->
      let b, _ = FrogMisc.Str.split ~by:' ' cmd in
      if b="$binary" then failwith "please provide a value for $binary";
      b
  in
  let unsat = get_str_ d "unsat" in
  let sat = get_str_ d "sat" in
  let unknown = get_str_ d "unknown" in
  let timeout = get_str_ d "timeout" in
  let memory = get_str_ d "memory" in
  { cmd; binary; unsat; sat; unknown; timeout; memory; }

let find_config config name =
  (* check that the prover is listed *)
  let provers = Conf.get_string_list ~default:[] config "provers" in
  if not (List.mem name provers)
    then failwith ("prover " ^ name ^ " not listed in config");
  build_from_config config name

(* make a list of provers from the given config *)
let of_config config =
  let provers = Conf.get_string_list ~default:[] config "provers" in
  List.fold_left
    (fun map p_name ->
      let prover = build_from_config config p_name in
      StrMap.add p_name prover map
    ) StrMap.empty provers

let run_cmd ?env ~timeout ~memory ~prover ~file () =
  Lwt_log.ign_debug_f "timeout: %d, memory: %d" timeout memory;
  let cmd = make_command ?env prover ~timeout ~memory ~file in
  let cmd = ["/bin/sh"; "-c"; cmd] in
  "/bin/sh", Array.of_list cmd

let run_exec ?env ~timeout ~memory ~prover ~file () =
  let cmd, args = run_cmd ?env ~timeout ~memory ~prover ~file () in
  Unix.execv cmd args

let run_proc ?env ~timeout ~memory ~prover ~file () =
  let cmd = run_cmd ?env ~timeout ~memory ~prover ~file () in
  (* slightly extended timeout *)
  let timeout_hard = (float timeout) +. 2. in
  Lwt_process.with_process_full ~timeout:timeout_hard cmd
    (fun p ->
      let res =
        let res_errcode =
          Lwt.map
            (function
              | Unix.WEXITED e -> e
              | Unix.WSIGNALED _
              | Unix.WSTOPPED _ -> 128)
            p#status
        in
        try%lwt
          let%lwt () = Lwt_io.close p#stdin in
          let%lwt res_out = Lwt_io.read p#stdout
          and res_err = Lwt_io.read p#stderr
          and res_errcode = res_errcode in
          Lwt.return (res_out, res_err, res_errcode)
        with e ->
          let%lwt res_errcode = res_errcode in
          Lwt_log.ign_debug_f ~exn:e "error while running %s"
            ([%show: (string * string array)] cmd);
          Lwt.return ("", "", res_errcode)
      in
      let open Lwt.Infix in
      (* pick the "timeout" message for this prover, if any *)
      let str = FrogMisc.Opt.get "timeout" prover.timeout in
      let timeout_fut = Lwt_unix.sleep timeout_hard >|= fun _ -> str, "", 0 in
      Lwt.pick [res; timeout_fut]
    )

module TPTP = struct
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
    let prover = find_config config prover in
    run_cmd ?env ~timeout ~memory ~prover ~file ()

end
