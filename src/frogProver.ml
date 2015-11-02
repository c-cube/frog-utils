
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Prover} *)

module Conf = FrogConfig
module StrMap = Map.Make(String)

type env = (string * string) array

type t = {
  cmd : string;  (* string that possible contains $file,
                    $memory and $timeout *)
  unsat : string option; (* regex for "unsat" *)
  sat : string option;
  unknown : string option;
  timeout : string option;
}

(* command ready to run in a shell *)
let make_command ?(env=[||]) p ~timeout ~memory ~file =
  let buf = Buffer.create 32 in
  let add_str s =
  Buffer.add_substitute buf
    (function
      | "memory" -> string_of_int memory
      | "timeout" | "time" -> string_of_int timeout
      | "file" -> file
      | _ -> raise Not_found
    ) s
  in
  add_str "ulimit -t \\$(( 1 + $time)) -v \\$(( 1000000 * $memory )); ";
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
  let cmd = Conf.get_string d "cmd" in
  let unsat = get_str_ d "unsat" in
  let sat = get_str_ d "sat" in
  let unknown = get_str_ d "unknown" in
  let timeout = get_str_ d "timeout" in
  { cmd; unsat; sat; unknown; timeout; }

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

let run_cmd ?env ?timeout ?memory ~config ~prover ~file =
  let p = find_config config prover in
  let timeout = match timeout with
    | Some t -> t
    | None -> Conf.get_int ~default:30 config "timeout"
  in
  let memory = match memory with
    | Some m -> m
    | None -> Conf.get_int ~default:1000 config "memory"
  in
  FrogDebug.debug "timeout: %d, memory: %d" timeout memory;
  let cmd = make_command ?env p ~timeout ~memory ~file in
  let cmd = ["/bin/sh"; "-c"; cmd] in
  "/bin/sh", Array.of_list cmd

let run_exec ?env ?timeout ?memory ~config ~prover ~file () =
  let cmd, args = run_cmd ?env ?timeout ?memory ~config ~prover ~file in
  Unix.execv cmd args


module TPTP = struct
  let make_command ?tptp p ~timeout ~memory ~file =
    let env = match tptp with
      | None -> [||]
      | Some f -> [| "TPTP", f |]
    in
    make_command ~env p ~timeout ~memory ~file

  let run_cmd ?tptp ?timeout ?memory ~config ~prover ~file =
    let env = match tptp with
      | Some f -> Some [| "TPTP", f |]
      | None ->
          match get_str_ config "TPTP" with
          | None -> None
          | Some f -> Some  [| "TPTP", f |]
    in
    run_cmd ?env ?timeout ?memory ~config ~prover ~file
end
