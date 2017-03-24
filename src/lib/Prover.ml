
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Prover} *)

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
  binary_deps: (string list [@default []]); (* additional list of binaries this depends on *)
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

let version_to_string = function
  | Tag s -> s
  | Git (b, c) -> Printf.sprintf "%s#%s" b c

let name p = p.name

let pp_name out p = Format.pp_print_string out p.name

exception Subst_not_found of string

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
        | s -> raise (Subst_not_found s))
      s
  in
  add_str "ulimit -t \\$(( 1 + $time)) -v \\$(( 1000 * $memory )); ";
  Array.iter
    (fun (key,value) -> add_str (key ^ "=" ^ value ^ " "))
    env;
  add_str cmd;
  Buffer.contents buf

let make_command ?env prover ~timeout ~memory ~file =
  let binary = prover.binary in
  try mk_cmd ?env ~binary ~timeout ~memory ~file prover.cmd
  with Subst_not_found s ->
    failwith (Printf.sprintf
        "cannot make command for prover %s: cannot find field %s" prover.name s)

module Map_name = CCMap.Make(struct
    type t = t_
    let compare a b = String.compare a.name b.name
  end)

module As_key = struct
  type t = t_

  let compare p1 p2 =
    let c = String.compare p1.name p2.name in
    if c<>0 then c else Pervasives.compare p1.version p2.version
end

module Map = CCMap.Make(As_key)
module Set = CCSet.Make(As_key)

(* HTML server *)
let to_html_name p =
  Html.cdata p.name

let to_html_fullname p =
  let module H = Html in
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
  let module R = Html.Record in
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

