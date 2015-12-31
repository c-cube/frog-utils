
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Config File} *)

exception Error of string

type table = {
  mutable tbl : TomlTypes.table;
  parent : t;
}
and t =
  | Empty
  | Table of table

let empty = Empty

let create () = Table {
  tbl=TomlTypes.Table.empty;
  parent=Empty;
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
      | _ -> raise Not_found
    ) s;
  Buffer.contents buf

(* try to parse a config file *)
let parse_or_empty file =
  match Toml.Parser.from_filename file with
  | `Error (msg, {Toml.Parser. source; line; column; _ }) ->
      Printf.eprintf "error trying to read config: %s at %s, line %d, col %d\n"
        msg source line column;
      empty
  | `Ok tbl -> Table {tbl; parent=Empty}

let parse_files l conf =
  let module P = Toml.Parser in
  let conf' =  List.fold_left
    (fun parent file ->
      try
        match parse_or_empty file with
        | Empty -> parent
        | Table {tbl; _} -> Table {tbl; parent; }
      with P.Error (msg, _) -> raise (Error msg)
    ) Empty l
  in
  match conf with
  | Empty -> conf'
  | Table {tbl; _} -> Table {tbl; parent=conf' }

exception WrongType

(* "generic" getter *)
let rec get_or ?default getter conf name =
  match conf with
  | Empty ->
      begin match default with
      | None -> raise Not_found
      | Some x -> x
      end
  | Table {tbl; parent} ->
      try
        getter (TomlTypes.Table.find (Toml.key name) tbl)
      with Not_found | WrongType ->
        get_or ?default getter parent name

type 'a getter = ?default:'a -> t -> string -> 'a

let get_table ?default conf name =
  let getter x = match x with
    | TomlTypes.TTable tbl -> Table {tbl; parent=Empty;}
    | _ -> raise WrongType
  in
  get_or ?default getter conf name

let get_bool ?default conf name =
  let f = function
    | TomlTypes.TBool b -> b
    | _ -> raise WrongType
  in
  get_or ?default f conf name

let get_int ?default conf name =
  let f = function
    | TomlTypes.TInt x -> x
    | _ -> raise WrongType
  in
  get_or ?default f conf name

let get_string ?default conf name =
  let get_string_exn = function
    | TomlTypes.TString x -> x
    | _ -> raise WrongType
  in
  get_or ?default get_string_exn conf name

let get_float ?default conf name =
  let f = function
    | TomlTypes.TFloat x -> x
    | _ -> raise WrongType
  in
  get_or ?default f conf name

let get_string_list ?default conf name =
  get_or ?default
    (fun s -> match s with
      | TomlTypes.TArray (TomlTypes.NodeString l) -> l
      | _ -> raise WrongType)
    conf name

