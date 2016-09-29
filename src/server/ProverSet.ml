
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Frog
open Prover

module StrMap = Misc.StrMap

let get_str_ d x =
  try Some (Config.get_string d x)
  with Config.FieldNotFound _ -> None

(* Internal function, do NOT export ! *)
let mk_cmd
    ?(env=[||])
    ?(binary="")
    ?(timeout=0)
    ?(memory=0)
    ?(file="")
    cmd =
  let buf = Buffer.create 32 in
  let add_str s =
    Buffer.add_substitute buf
      (function
        | "memory" -> string_of_int memory
        | "timeout" | "time" -> string_of_int timeout
        | "file" -> file
        | "binary" -> binary
        | _ -> raise Not_found)
      s
  in
  (* XXX: seems to ake zombie processes?
     add_str "ulimit -t \\$(( 1 + $time)) -v \\$(( 1000000 * $memory )); ";
  *)
  Array.iter
    (fun (key,value) -> add_str (key ^ "=" ^ value ^ " "))
    env;
  add_str cmd;
  Buffer.contents buf

(* obtain the current commit name *)
let get_cmd_out cmd =
  let open Lwt.Infix in
  Lwt_main.run @@
  Lwt_process.with_process_in (Lwt_process.shell cmd)
    (fun p -> Misc_unix.File.read_all p#stdout >|= String.trim)

let get_commit dir : string =
  get_cmd_out (
    Printf.sprintf "git -C %s rev-parse HEAD" dir)

let get_branch dir : string =
  get_cmd_out (
    Printf.sprintf "git -C %s branch | grep '*' | cut -d ' ' -f2" dir)

(* recover description of prover from config file *)
let build_from_config config name =
  let d =
    try Config.get_table config name
    with Config.FieldNotFound _ ->
      failwith ("could not find prover " ^ name ^ " in config")
  in
  let cmd = Config.get_string d "cmd" |> String.trim in
  let binary =
    try Config.get_string d "binary"
    with Config.FieldNotFound _ ->
      let b, _ = Misc.Str.split ~by:' ' cmd in
      if b = "$binary" then
        failwith ("please provide $binary value for prover " ^ name)
      else
        b
  in
  let version =
    match Config.get_string d "version" with
    | exception Config.FieldNotFound _ ->
      Tag "dev"
    | s ->
      begin match Misc.Str.split ~by:':' s with
        | "git", dir ->
          Git (get_branch dir, get_commit dir)
        | "cmd", cmd ->
          Tag (get_cmd_out @@ mk_cmd ~binary cmd)
        | _ -> Tag s
        | exception Not_found -> Tag s
      end
  in
  let unsat = get_str_ d "unsat" in
  let sat = get_str_ d "sat" in
  let unknown = get_str_ d "unknown" in
  let timeout = get_str_ d "timeout" in
  let memory = get_str_ d "memory" in
  { name; version; cmd; binary; unsat; sat; unknown; timeout; memory; }

let find_config config name =
  (* check that the prover is listed *)
  let provers = Config.get_string_list ~default:[] config "provers" in
  if not (List.mem name provers)
  then failwith ("prover " ^ name ^ " not listed in config");
  build_from_config config name

(* make a list of provers from the given config *)
let of_config config =
  let provers = Config.get_string_list ~default:[] config "provers" in
  List.fold_left
    (fun map p_name ->
       let prover = build_from_config config p_name in
       StrMap.add p_name prover map
    ) StrMap.empty provers

