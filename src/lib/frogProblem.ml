
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Result
open FrogDB

module W = FrogWeb
module Res = FrogRes

[@@@warning "-39"]
type t = {
  name: string;  (* filename *)
  expected: Res.t; (* result expected *)
} [@@deriving yojson]
[@@@warning "+39"]

let same_name t1 t2 = t1.name = t2.name
let compare_name t1 t2 = Pervasives.compare t1.name t2.name

(* regex + mark *)
let m_unsat_, unsat_ = Re.(str "unsat" |> no_case |> mark)
let m_sat_, sat_ = Re.(str "sat" |> no_case |> mark)
let m_unknown_, unknown_ = Re.(str "unknown" |> no_case |> mark)
let m_timeout_, timeout__ = Re.(str "timeout" |> no_case |> mark)
let m_error_, error_ = Re.(alt [str "error"; str "fail"] |> no_case |> mark)

(* "^ #expect: (unsat|sat|unknown|error)", basically *)
let re_expect_ =
  Re.(seq
        [ alt (List.map no_case [str "expect:"; str "expected:"]) ; rep blank;
          alt [unsat_; sat_; unknown_; error_] ]
      |> compile
     )

(* what is expected? *)
let find_expected_ ~default_expect ~file () =
  let%lwt content = FrogMisc.File.with_in ~file FrogMisc.File.read_all in
  try%lwt
    let g = Re.exec re_expect_ content in
    if Re.marked g m_unsat_ then Lwt.return Res.Unsat
    else if Re.marked g m_sat_ then Lwt.return Res.Sat
    else if Re.marked g m_unknown_ then Lwt.return Res.Unknown
    else if Re.marked g m_timeout_ then Lwt.return Res.Timeout
    else if Re.marked g m_error_ then Lwt.return Res.Error
    else Lwt.fail (Failure "could not parse the content of the `expect:` field")
  with Not_found ->
    match default_expect with
      | Some r -> Lwt.return r
      | None -> Lwt.fail (Failure "could not find the `expect:` field")

let make ~default_expect ~file () =
  try%lwt
    Lwt_log.ign_debug_f "convert `%s` into problem..." file;
    let%lwt res = find_expected_ ~default_expect ~file () in
    let pb = {
      name=file;
      expected=res;
    } in
    Lwt.return (Ok pb)
  with e ->
    Lwt.return
      (Error (
          Format.asprintf "could not find expected res for %s: %s"
            file (Printexc.to_string e)))

let compare_res pb res =
  match pb.expected, res with
  | Res.Unsat, Res.Unsat
  | Res.Sat, Res.Sat
  | Res.Timeout, Res.Timeout
  | Res.Unknown, Res.Unknown
  | Res.Error, Res.Error -> `Same
  | (Res.Sat | Res.Unsat | Res.Error), (Res.Unknown | Res.Timeout) -> `Disappoint
  | Res.Timeout, Res.Unknown
  | Res.Unknown, Res.Timeout
  | (Res.Unsat | Res.Error), Res.Sat
  | (Res.Sat | Res.Error), Res.Unsat
  | (Res.Sat | Res.Unknown | Res.Timeout | Res.Unsat), Res.Error ->
    `Mismatch
  | (Res.Unknown | Res.Timeout), (Res.Sat | Res.Unsat) ->
    `Improvement

let print out p =
  Format.fprintf out "@[<h>%s (expect: %a)@]" p.name Res.print p.expected

let to_string p = FrogMisc.Fmt.to_string print p

let maki =
  let module V = Maki.Value in
  V.map ~descr:"problem"
    (fun p -> p.name, p.expected)
    (fun (name, expected) -> {name;expected})
    (V.pair V.file Res.maki)

(* Hashes of problems *)
let hash p : string =
  Sha1.string p.name |> Sha1.to_hex

(* DB management *)
let db_init t =
  FrogDB.exec t
    "CREATE TABLE IF NOT EXISTS problems (
      hash STRING PRIMARY KEY,
      name STRING,
      expected STRING)"
    (fun _ -> ())

let find_aux row = match row with
  | [| FrogDB.D.BLOB name; FrogDB.D.BLOB s |] ->
    let expected = FrogRes.of_string s in
    { name; expected; }
  | _ -> assert false

let find db h =
  FrogDB.exec1 db
    "SELECT name, expected FROM problems WHERE hash=?"
    (FrogDB.D.string h)
    (fun c -> match FrogDB.Cursor.head c with
       | Some row -> Some (find_aux row)
       | None -> None)

let find_all db =
  FrogDB.exec db
    "SELECT name, expected FROM problems"
    (fun c ->
       FrogDB.Cursor.to_list_rev c
       |> List.map find_aux)

let db_add db t =
  FrogDB.exec_a db
    "INSERT OR IGNORE INTO problems(hash,name,expected) VALUES (?,?,?)"
    [| FrogDB.D.string (hash t)
     ; FrogDB.D.string t.name
     ; FrogDB.D.string (FrogRes.to_string t.expected)
    |]
    (fun _ -> ())

(* HTML server *)
let to_html_name p = W.Html.string (Filename.basename p.name)

let k_uri = W.HMap.Key.create ("uri_of_problem", fun _ -> Sexplib.Sexp.Atom "")
let k_add = W.HMap.Key.create ("add_problem", fun _ -> Sexplib.Sexp.Atom "")

let add_server s =
  (* md5(problem.name) -> problem *)
  let uri_of_pb pb = Uri.make ~path:("/problem/" ^ hash pb) () in
  let add_pb pb = db_add (W.Server.db s) pb in
  let handler req =
    let open Opium.Std in
    let h = param req "hash" in
    match find (W.Server.db s) h with
    | Some pb ->
      (* read the problem itself *)
      Lwt_io.with_file ~mode:Lwt_io.input pb.name
        FrogMisc.File.read_all
      >>= fun content ->
      W.Html.list
        [ W.Html.h2 (to_html_name pb)
        ; W.Record.start
          |> W.Record.add "path" (W.Html.string pb.name)
          |> W.Record.add "expected" (FrogRes.to_html pb.expected)
          |> W.Record.add "contents" (W.pre (W.Html.string content))
          |> W.Record.close
        ]
      |> W.Server.return_html ~title:"problem"
    | None ->
      let code = Cohttp.Code.status_of_code 404 in
      let h =
        W.Html.string (Printf.sprintf "could not find problem %s" h) in
      W.Server.return_html ~code h
  in
  W.Server.set s k_uri uri_of_pb;
  W.Server.set s k_add add_pb;
  W.Server.add_route s "/problem/:hash" handler

