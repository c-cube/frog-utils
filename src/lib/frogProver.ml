
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Prover} *)

open FrogDB
module StrMap = Map.Make(String)

[@@@warning "-39"]
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
[@@@warning "+39"]

let string_opt =
  Maki.Value.(
    map
      (function Some s -> s | None -> "")
      (function "" -> None | s -> Some s)
      string
  )

let maki =
  Maki.Value.(
    map
      (fun t -> ((t.binary, t.cmd), (t.unsat, t.sat), (t.unknown, t.timeout, t.memory)))
      (fun ((binary, cmd), (unsat, sat), (unknown, timeout, memory)) ->
         { binary; cmd; unsat; sat; unknown; timeout; memory })
      (triple
         (pair program string)
         (pair string_opt string_opt)
         (triple string_opt string_opt string_opt)
      )
  )

let get_str_ d x =
  try Some (FrogConfig.get_string d x)
  with FrogConfig.Field_not_found _ -> None

(* recover description of prover from config file *)
let build_from_config config name =
  let d =
    try FrogConfig.get_table config name
    with FrogConfig.Field_not_found _ ->
      failwith ("could not find prover " ^ name ^ " in config")
  in
  let cmd = FrogConfig.get_string d "cmd" |> String.trim in
  let binary =
    try FrogConfig.get_string d "binary"
    with FrogConfig.Field_not_found _ ->
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

(* DB interaction *)
let hash t =
  Sha1.string @@ Lwt_main.run (Maki.Value.to_string maki t)
  |> Sha1.to_hex

let db_init t =
  Sqlexpr.execute t [%sql
    "CREATE TABLE IF NOT EXISTS provers (
        hash STRING PRIMARY KEY,
        contents STRING)"]

let find_aux s =
  match of_yojson (Yojson.Safe.from_string s) with
  | `Ok t -> t
  | `Error _ -> assert false

let find db hash =
  match Sqlexpr.select_one_maybe db [%sqlc
          "SELECT @s{contents} FROM provers WHERE hash=%s"] hash with
  | Some s -> Some (find_aux s)
  | None -> None

let find_all db =
  Sqlexpr.select_f db find_aux [%sqlc "SELECT @s{contents} FROM provers"]

let db_add db t =
  Sqlexpr.execute db [%sqlc "INSERT OR IGNORE INTO provers(hash,contents) VALUES (%s,%s)"]
    (hash t) (Yojson.Safe.to_string (to_yojson t))

(* HTML server *)
let to_html_name p =
  FrogWeb.Html.string (Filename.basename p.binary)

let to_html_full p =
  let module R = FrogWeb.Record in
  R.start
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
      FrogWeb.Server.return_html ~title:prover.binary
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

