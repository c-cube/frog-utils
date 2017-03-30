(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Config File} *)

exception Error of string

type table = {
  file : string;
  tbl  : TomlTypes.table;
  parent : node;
}
and node =
  | Empty
  | Table of table

type t = {
  path : string;
  node : node;
}

let mk ?(path="") node = { path; node; }

let empty = mk Empty

let create () = mk @@ Table {
    file = "";
    tbl = TomlTypes.Table.empty;
    parent= Empty;
  }

(* obtain $HOME *)
let get_home () =
  let ic = Unix.open_process_in "echo $HOME" in
  let home = input_line ic in
  close_in ic;
  home

let interpolate_home s =
  let h = lazy (get_home ()) in
  let buf = Buffer.create (String.length s) in
  Buffer.add_substitute buf
    (function
      | "HOME" | "home" -> Lazy.force h
      | s -> failwith ("couldn't find variable: " ^ s)
    ) s;
  Buffer.contents buf

(* try to parse a config file *)
let parse_node file =
  match Toml.Parser.from_filename file with
  | `Ok tbl ->
    Table {file; tbl; parent = Empty; }
  | `Error (msg, {Toml.Parser. source; line; column; _ }) ->
    Format.eprintf "%s@." msg;
    Empty
  | exception e ->
    Printf.eprintf "error trying to read config: %s" (Printexc.to_string e);
    Empty

let parse_or_empty file = mk (parse_node file)

let parse_files l conf =
  let module P = Toml.Parser in
  let conf' =  List.fold_left
      (fun parent file ->
         try
           match parse_node file with
           | Empty ->
             parent
           | Table { file; tbl; parent = Empty } ->
             Table { file; tbl; parent; }
           | _ -> assert false
         with P.Error (msg, _) -> raise (Error msg)
      ) Empty l
  in
  if conf.path <> "" then
    failwith "FrogConfig.parse_files: start config should have an empty path";
  let node = match conf.node with
    | Empty -> conf'
    | Table { file; tbl; parent = Empty } ->
      Table { file; tbl; parent = conf' }
    | _ -> failwith "FrogConfig.parse_files: start config should have an empty parent"
  in
  mk node


(* Exceptions for getters *)

exception WrongType of string * string * string (** file, path, field name *)
exception FieldNotFound of string * string    (** path and field name *)

let () = Printexc.register_printer
    (function
      | WrongType (file, path, n) ->
        Some (Printf.sprintf
                "In file '%s', section '%s':\nfield `%s` has the wrong type"
                file path n)
      | FieldNotFound (path, n) ->
        Some (Printf.sprintf
                "In section `%s`, could not find field `%s`" path n)
      | _ -> None)


(* "generic" getter *)
let rec get_or ?default getter conf name =
  let rec aux = function
    | Empty ->
      begin match default with
        | None -> raise (FieldNotFound (conf.path, name))
        | Some x -> x
      end
    | Table {file; tbl; parent} ->
      match getter (TomlTypes.Table.find (Toml.key name) tbl) with
      | None -> raise (WrongType (file, conf.path, name))
      | Some res -> res
      | exception Not_found ->
        aux parent
  in
  aux conf.node

type 'a getter = ?default:'a -> t -> string -> 'a

let get_bool ?default conf name =
  let f = function
    | TomlTypes.TBool b -> Some b
    | _ -> None
  in
  get_or ?default f conf name

let get_int ?default conf name =
  let f = function
    | TomlTypes.TInt x -> Some x
    | _ -> None
  in
  get_or ?default f conf name

let get_string ?default conf name =
  let get_string_exn = function
    | TomlTypes.TString x -> Some x
    | _ -> None
  in
  get_or ?default get_string_exn conf name

let get_string_opt conf name =
  try Some (get_string conf name)
  with FieldNotFound _ -> None

let get_float ?default conf name =
  let f = function
    | TomlTypes.TFloat x -> Some x
    | _ -> None
  in
  get_or ?default f conf name

let get_string_list ?default conf name =
  let f = function
    | TomlTypes.TArray (TomlTypes.NodeString l) -> Some l
    | _ -> None
  in
  get_or ?default f conf name

let get_table ?default conf name =
  let rec aux = function
  | Empty -> Empty
  | Table {file; tbl; parent} ->
    match TomlTypes.Table.find (Toml.key name) tbl with
    | TomlTypes.TTable tbl ->
      Table {file; tbl; parent = aux parent; }
    | _ ->
      raise (WrongType (file, conf.path, name))
    | exception Not_found ->
      aux parent
  in
  let path =
    if conf.path = "" then name
    else Format.sprintf "%s.%s" conf.path name
  in
  match aux conf.node with
  | Empty ->
    begin match default with
      | None -> raise (FieldNotFound (conf.path, name))
      | Some res -> res
    end
  | node -> mk ~path node
