
(* This file is free software, part of frog-utils. See file "license" for more details. *)

module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Id)
module S = Sqlexpr

type t = S.db
type prover = Int64.t
type problem = Int64.t

(*
  Current layout of the Sqlite DB is as follows:
   - a table 'provers' which establish a relation id <-> prover
   - a table 'problem' which establish a relation id <-> problem
   - a table 'results' with the results of all benchs/tests, in this table,
      provers and problems are referred to by their id.
*)

let init t =
  S.execute t [%sql
    "CREATE TABLE IF NOT EXISTS provers (id INT PRIMARY KEY AUTOINCREMENT, name STRING UNIQUE, contents STRING)"];
  S.execute t [%sql
    "CREATE TABLE IF NOT EXISTS problems (id INT PRIMARY KEY AUTOINCREMENT, name STRING UNIQUE, expected STRING)"];
  S.execute t [%sql
    "CREATE TABLE IF NOT EXISTS results (id INT PRIMARY KEY AUTOINCREMENT, prover INT, problem INT
                                        ,res STRING, stdout STRING, stderr STRING, errcode INT
                                        ,rtime REAL, utime REAL, stime REAL)"];
  (* TODO: Add INDEX creation *)
  ()

let open_db path =
  let t = S.open_db path in
  let () = init t in
  t

(* Prover manipulation *)
let prover_uniq t = t.FrogProver.binary

let prover_id db t =
  S.select_one_maybe db [%sqlc "SELECT @L{id} FROM provers WHERE name=%s"] (prover_uniq t)

let find_prover db id =
  let s = S.select_one db [%sqlc "SELECT @s{contents} FROM PROVERS WHERE id=%d"] id in
  match FrogProver.of_yojson (Yojson.Safe.from_string s) with
  | `Ok t -> t
  | `Error _ -> assert false

let add_prover db t =
  S.execute db [%sqlc "INSERT OR IGNORE INTO provers(name,contents) VALUES (%s,%s)"]
    (prover_uniq t) (Yojson.Safe.to_string (FrogProver.to_yojson t))

(* Problem manipulation *)
let problem_uniq t = t.FrogProblem.name

let problem_id db t =
  S.select_one_maybe db [%sqlc "SELECT @L{id} FROM problems WHERE name=%s"] (problem_uniq t)

let find_problem db id =
  let name, s = S.select_one db [%sqlc "SELECT (@s{name},@s{expected}) FROM problems WHERE id=%d "] id in
  let expected = FrogRes.of_string s in
  { FrogProblem.name; expected; }

let add_problem db t =
  S.execute db [%sqlc "INSERT OR IGNORE INTO problems(name,expected) VALUES (%s,%s)"]
    (problem_uniq t) (FrogRes.to_string t.FrogProblem.expected)


(* Add bench/test results *)
let add_result db prover t =
  let open FrogMap in
  let () = add_problem db t.problem in
  match problem_id db t.problem with
  | Some problem ->
    S.execute db [%sql "INSERT INTO results VALUES (%L,%L,%s,%s,%s,%d,%f,%f,%f)"]
      prover problem (FrogRes.to_string t.res) t.stdout t.stderr t.errcode t.rtime t.utime t.stime
  | None -> assert false (* we just added the problem before, so this case shouldn't happen *)

let add_list db t l =
  let () = add_prover db t in
  match prover_id db t with
  | Some id ->
    S.transaction db (fun db ->
        List.iter (add_result db id) l)
  | None -> assert false (* once again, shouldn't happen becaue we just inserted the prover *)


