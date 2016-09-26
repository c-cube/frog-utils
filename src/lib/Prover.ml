
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Prover} *)

open FrogDB
module StrMap = Map.Make(String)

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
  try Some (FrogConfig.get_string d x)
  with FrogConfig.FieldNotFound _ -> None

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
    (fun p -> FrogMisc.File.read_all p#stdout >|= String.trim)

let get_commit dir : string =
  get_cmd_out (
    Printf.sprintf "git -C %s rev-parse HEAD" dir)

let get_branch dir : string =
  get_cmd_out (
    Printf.sprintf "git -C %s branch | grep '*' | cut -d ' ' -f2" dir)

(* recover description of prover from config file *)
let build_from_config config name =
  let d =
    try FrogConfig.get_table config name
    with FrogConfig.FieldNotFound _ ->
      failwith ("could not find prover " ^ name ^ " in config")
  in
  let cmd = FrogConfig.get_string d "cmd" |> String.trim in
  let binary =
    try FrogConfig.get_string d "binary"
    with FrogConfig.FieldNotFound _ ->
      let b, _ = FrogMisc.Str.split ~by:' ' cmd in
      if b = "$binary" then
        failwith ("please provide $binary value for prover " ^ name)
      else
        b
  in
  let version =
    match FrogConfig.get_string d "version" with
    | exception FrogConfig.FieldNotFound _ ->
      Tag "dev"
    | s ->
      begin match FrogMisc.Str.split ~by:':' s with
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
  let provers = FrogConfig.get_string_list ~default:[] config "provers" in
  if not (List.mem name provers)
  then failwith ("prover " ^ name ^ " not listed in config");
  build_from_config config name


(* make a list of provers from the given config *)
let of_config config =
  let provers = FrogConfig.get_string_list ~default:[] config "provers" in
  List.fold_left
    (fun map p_name ->
       let prover = build_from_config config p_name in
       StrMap.add p_name prover map
    ) StrMap.empty provers

let make_command ?env prover ~timeout ~memory ~file =
  let binary = prover.binary in
  mk_cmd ?env ~binary ~timeout ~memory ~file prover.cmd

(* DB interaction *)
let hash t =
  Sha1.string @@
  (Printf.sprintf "%s:%s" t.name (version_to_string t.version))
  |> Sha1.to_hex

let db_init t =
  FrogDB.exec t
    "CREATE TABLE IF NOT EXISTS provers (
        hash STRING PRIMARY KEY,
        contents STRING)"
    (fun _ -> ())

let find_aux (r:FrogDB.row) = match r with
  | [| FrogDB.D.BLOB s |] ->
    begin match of_yojson (Yojson.Safe.from_string s) with
      | Result.Ok t -> t
      | Result.Error _ -> assert false
    end
  | _ -> assert false

let find db hash =
  FrogDB.exec_a db "SELECT contents FROM provers WHERE hash=?"
    [| FrogDB.D.string hash |]
    (fun c -> match FrogDB.Cursor.head c with
       | Some r -> Some (find_aux r)
       | None -> None)

let find_all db =
  FrogDB.exec db "SELECT contents FROM provers"
    (fun c ->
       Cursor.to_list_rev c
       |> List.map find_aux)

let db_add db t =
  FrogDB.exec_a db
    "INSERT OR IGNORE INTO provers(hash,contents) VALUES (?,?)"
    [| FrogDB.D.string (hash t)
       ; FrogDB.D.string (Yojson.Safe.to_string (to_yojson t))
    |]
    (fun _ -> ())

(* HTML server *)
let to_html_name p =
  FrogWeb.Html.string p.name

let to_html_fullname p =
  match p.version with
  | Tag s ->
    FrogWeb.Html.string (Format.sprintf "%s %s" p.name s)
  | Git (branch, commit) ->
    FrogWeb.Html.list [
      FrogWeb.Html.string (
        Format.sprintf "%s@@%s" p.name branch);
      FrogWeb.Html.br @@ FrogWeb.Html.string (
        Format.sprintf "%s.." (String.sub commit 0 15));
    ]

let to_html_full p =
  let module R = FrogWeb.Record in
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

let k_uri = FrogWeb.HMap.Key.create ("uri_of_prover", fun _ -> Sexplib.Sexp.Atom "")
let k_add = FrogWeb.HMap.Key.create ("add_prover", fun _ -> Sexplib.Sexp.Atom "")

let add_server s =
  let uri_of_prover p = Uri.make ~path:("/prover/" ^ hash p) () in
  let add_prover p = db_add (FrogWeb.Server.db s) p in
  let handle req =
    let open Opium.Std in
    let h = param req "hash" in
    match find (FrogWeb.Server.db s) h with
    | Some prover ->
      FrogWeb.Server.return_html ~title:prover.name
        (FrogWeb.Html.list [
            FrogWeb.Html.h2 (to_html_name prover);
            to_html_full prover;
          ])
    | None ->
      let code = Cohttp.Code.status_of_code 404 in
      FrogWeb.Server.return_html ~code (FrogWeb.Html.string "prover not found")
  in
  FrogWeb.Server.set s k_add add_prover;
  FrogWeb.Server.set s k_uri uri_of_prover;
  FrogWeb.Server.add_route s "/prover/:hash" handle;
  ()

