
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Prover} *)

module StrMap = Misc.StrMap

type version =
  | Tag of string
  | Git of string * string  (* branch & commit hash *)
  [@@deriving yojson]

[@@@warning "-39"]
type t = {
  (* Prover identification *)
  name : string;
  version : version;

  (* Pover execution *)
  binary: string;       (* name of the program itself *)
  cmd: string;          (* the command line to run.
                           possibly contains $binary, $file, $memory and $timeout *)

  (* Result analysis *)
  unsat   : string option;  (* regex for "unsat" *)
  sat     : string option;  (* regex for "sat" *)
  unknown : string option;  (* regex for "unknown" *)
  timeout : string option;  (* regex for "timeout" *)
  memory  : string option;  (* regex for "out of memory" *)
} [@@deriving yojson]
[@@@warning "+39"]

type t_ = t

let equal p1 p2 = p1.name = p2.name

let string_opt =
  Maki.Value.(
    map
      (function Some s -> s | None -> "")
      (function "" -> None | s -> Some s)
      string
  )

let maki_version =
  Maki.Value.marshal "frogprover.version"

let maki =
  Maki.Value.(
    map
      (fun t -> (
           (t.name, t.version),
           (t.binary, t.cmd),
           (t.unsat, t.sat),
           (t.unknown, t.timeout, t.memory)
         ))
      (fun ((name, version), (binary, cmd), (unsat, sat), (unknown, timeout, memory)) ->
         { name; version; binary; cmd; unsat; sat; unknown; timeout; memory })
      (quad
         (pair string maki_version)
         (pair program string)
         (pair string_opt string_opt)
         (triple string_opt string_opt string_opt)
      )
  )

let version_to_string = function
  | Tag s -> s
  | Git (b, c) -> Printf.sprintf "%s#%s" b c

let get_str_ d x =
  try Some (Config.get_string d x)
  with Config.FieldNotFound _ -> None

let name p = p.name

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
  (* XXX: seems to make zombie processes?
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
    (fun p -> Misc.File.read_all p#stdout >|= String.trim)

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

let make_command ?env prover ~timeout ~memory ~file =
  let binary = prover.binary in
  mk_cmd ?env ~binary ~timeout ~memory ~file prover.cmd

let hash t =
  Sha1.string @@
  (Printf.sprintf "%s:%s" t.name (version_to_string t.version))
  |> Sha1.to_hex

module Map = struct
  include Map.Make(struct
    type t = t_

    let compare p1 p2 =
      let c = String.compare p1.name p2.name in
      if c<>0 then c else Pervasives.compare p1.version p2.version
  end)

  let to_list m = fold (fun prover res acc -> (prover,res)::acc) m []

  let of_list l = List.fold_left (fun acc (p,r) -> add p r acc) empty l
end

(* HTML server *)
let to_html_name p =
  Web.Html.cdata p.name

let to_html_fullname p =
  let module H = Web.Html in
  match p.version with
  | Tag s ->
    H.div [H.pcdata (Format.sprintf "%s %s" p.name s)]
  | Git (branch, commit) ->
    H.ul [
      H.li [H.pcdata (
          Format.sprintf "%s@@%s" p.name branch)];
      H.li [H.pcdata (
        Format.sprintf "%s.." (String.sub commit 0 15))];
    ]

let to_html_full p =
  let module R = Web.Record in
  R.start
  |> R.add_string "version" (version_to_string p.version)
  |> R.add_string "binary" p.binary
  |> R.add_string ~raw:true "cmd" p.cmd
  |> R.add_string_option ~raw:true "unsat" p.unsat
  |> R.add_string_option ~raw:true "sat" p.sat
  |> R.add_string_option ~raw:true "unknown" p.unknown
  |> R.add_string_option ~raw:true "timeout" p.timeout
  |> R.add_string_option ~raw:true "out of space" p.memory
  |> R.close

