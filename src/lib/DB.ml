
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Utils for SQLite} *)

exception RcError of Sqlite3.Rc.t

let () = Printexc.register_printer
    (function RcError rc -> Some ("sqlite error: " ^ Sqlite3.Rc.to_string rc)
            | _ -> None)

let check_ret = function
  | Sqlite3.Rc.DONE
  | Sqlite3.Rc.OK -> ()
  | rc -> raise (RcError rc)

module D = struct
  type t = Sqlite3.Data.t =
    | NONE
    | NULL
    | INT of int64
    | FLOAT of float
    | TEXT of string
    | BLOB of string

  let int i = INT (Int64.of_int i)
  let string s = BLOB s

  let as_int = function
    | INT i -> Int64.to_int i
    | _ -> failwith "expected int"

  let as_string = function
    | TEXT s
    | BLOB s -> s
    | _ -> failwith "expected int"
end

type data = Sqlite3.Data.t

type row = data array

type rc = Sqlite3.Rc.t (** Error code *)

type stmt = string (** A query *)

type t = Sqlite3.db
(** A connection to some Sqlite DB *)

module Cursor = struct
  type t = {
    stmt: Sqlite3.stmt;
    mutable cur: D.t array option;
    mutable closed: bool;
  }

  let next_ stmt = match Sqlite3.step stmt with
    | Sqlite3.Rc.DONE -> None
    | Sqlite3.Rc.ROW ->
      let row = Sqlite3.row_data stmt in
      Some row
    | rc -> raise (RcError rc)

  let make stmt =
    { stmt;
      cur = next_ stmt;
      closed = false;
    }

  let close c =
    if not c.closed then (
      c.closed <- true;
      check_ret (Sqlite3.finalize c.stmt)
    )

  (* next value in the cursor *)
  let next c = match c.cur with
    | None -> None
    | Some _ ->
      let res = c.cur in
      let next = next_ c.stmt in
      c.cur <- next;
      if next = None then close c;
      res

  let junk c = ignore (next c)

  let head c = c.cur

  let head_exn c = match c.cur with
    | None -> failwith "empty cursor"
    | Some d -> d

  let rec iter ~f c = match c.cur with
    | None -> ()
    | Some res -> f res; junk c; iter ~f c

  (* convert a cursor into a list of answers *)
  let to_list_rev
    : t -> _ list
    = fun c ->
      let rec aux acc c = match next c with
        | None -> acc
        | Some d -> aux (d::acc) c
      in
      aux [] c

  let to_list c = List.rev (to_list_rev c)
end

(* on "busy", wait this amound of milliseconds before failing *)
let setup_timeout db =
  Sqlite3.busy_timeout db 300

let close_ db = ignore (Sqlite3.db_close db)

let open_ str =
  let db = Sqlite3.db_open str in
  setup_timeout db;
  Gc.finalise close_ db;
  db

let finally_ h f x =
  try
    let y = f x in
    h x;
    y
  with e ->
    h x;
    raise e

let with_open str f =
  let db = open_ str in
  let close_ x = ignore (Sqlite3.db_close x) in
  finally_ close_ f db

let create ?(db_init=[]) ~db_path () =
  let db = open_ (FrogConfig.interpolate_home db_path) in
  List.iter ((|>) db) db_init;
  db

let finally_close_ f c = finally_ Cursor.close f c

(* execute statement, return cursor *)
let exec db stmt f =
  let stmt = Sqlite3.prepare db stmt in
  assert (Sqlite3.bind_parameter_count stmt = 0);
  let c = Cursor.make stmt in
  finally_close_ f c

(* execute statement parametrized by the array of arguments *)
let exec_a db stmt a f =
  let stmt = Sqlite3.prepare db stmt in
  if Sqlite3.bind_parameter_count stmt <> Array.length a
  then invalid_arg
      (Format.sprintf "exec_a: wrong number of arguments, expected %d, got %d"
         (Sqlite3.bind_parameter_count stmt) (Array.length a));
  Array.iteri (fun i x -> ignore (Sqlite3.bind stmt (i+1) x)) a;
  let c = Cursor.make stmt in
  finally_close_ f c

(* execute statement with 1 param, return rows *)
let exec1 db stmt x f =
  exec_a db stmt [| x |] f

