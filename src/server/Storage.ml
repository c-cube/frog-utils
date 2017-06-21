
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Storage on Disk} *)

open Frog

module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Id)
module S = Sqlexpr

type id = string

type t = S.db

(* Database Layout

   There are 2 kinds of tables, tables that store content, and tables there to
   make the link between the tables' contents.

   The contents tables are:
   - "results"    containing the actual results of a prover on a problem
                  this table expliclty stores all data on a result in columns
                  instead of serializing results as blobs, so that queries can be made
                  against it using SQL
   - "blobs"      containing arbitrary blobs of information, indexed by hash
                  the table is used to store full information on benchamrks

   The linking tables are:
   - "benchmarks" containing the list of all benchs done
   - "bench_res"  linking benchmark ids with results ids
   - "provers"    containing the list of known provers, along with some metadata to match on
   - "problems"   containing the list of known problems, along with some metadata to match on

   For the full specification of how the tables are arranged, see the CREATE
   statements below.

   As for differneitating the TEXT and BLOB types, the tables use the following rules:
   - TEXT is used for every thing one might want to index/query on
   - BLOB is used for data that is serialized and should *NOT* be matched
*)

module Hash = struct

  type t = id

  let string s = Sha1.to_hex @@ Sha1.string s
  let yojson y = string @@ Yojson.Safe.to_string y

  let prover p = yojson @@ Prover.to_yojson p
  let problem p = yojson @@ Problem.to_yojson p
  let pv_result r = yojson @@ Event.result_to_yojson Prover.to_yojson r
end

let version_to_string = function
  | Prover.Tag s -> s
  | Prover.Git (branch, commit) -> Printf.sprintf "%s#%s" branch commit

let string_to_version s =
  match Misc.Str.split ~by:'#' s with
  | exception Not_found -> Prover.Tag s
  | branch, commit -> Prover.Git (branch, commit)

(* ---  Database Tables     --- *)

(* The blobs table stores arbitrary blobs of usually serialised data *)
let init_blobs db =
  S.execute db [%sqlc
    "CREATE TABLE IF NOT EXISTS blobs (
      id TEXT PRIMARY KEY ON CONFLICT IGNORE,
      data BLOB
    )"]

(* temporarily ignore errors. TODO: handler errors correctly. *)
let get_blob db (decoder,hash) =
  let r = S.select_one db [%sqlc "SELECT @S{data} from blobs WHERE id=%s"] hash in
  match decoder r with
  | Ok x -> x
  | Error _ -> assert false

let write_blob db hash value =
  S.execute db [%sqlc "INSERT INTO blobs(id,data) VALUES(%s,%S)"] hash value

let write_json db hash json =
  let raw = Yojson.Safe.to_string json in
  write_blob db hash raw


(* The results table stores preyy much all of a results info in separate columns,
   to allow for querying. An index is created on all columns exceptt
   id (because it is already indexed), stdout/stderr (because what is
   stored are the hash of stored blobs), and stime. *)
let init_results db =
  let () = S.execute db [%sqlc
    "CREATE TABLE IF NOT EXISTS results (
      id TEXT PRIMARY KEY ON CONFLICT ABORT,
      prover  TEXT,
      problem TEXT,
      status  TEXT,
      errcode INT,
      stdout  TEXT,
      stderr  TEXT,
      rtime   REAL,
      utime   REAL,
      stime   REAL
    )"] in
  let () = S.execute db [%sqlc
      "CREATE INDEX IF NOT EXISTS results_index_prover ON results(prover)"] in
  let () = S.execute db [%sqlc
      "CREATE INDEX IF NOT EXISTS results_index_problem ON results(problem)"] in
  let () = S.execute db [%sqlc
      "CREATE INDEX IF NOT EXISTS results_index_status ON results(status)"] in
  let () = S.execute db [%sqlc
      "CREATE INDEX IF NOT EXISTS results_index_errcode ON results(errcode)"] in
  let () = S.execute db [%sqlc
      "CREATE INDEX IF NOT EXISTS results_index_rtime ON results(rtime)"] in
  let () = S.execute db [%sqlc
      "CREATE INDEX IF NOT EXISTS results_index_utime ON results(utime)"] in
  ()

let mem_result db id =
  match S.select db [%sqlc "SELECT @d{id} FROM results WHERE id=%s"] id with
  | [] -> false
  | _ -> true

let get_result db id =
  let (pv_id, pb_id, errcode, out_id, err_id, rtime, utime, stime) =
    S.select_one db [%sqlc
      "SELECT @s{prover},@s{problem},@d{errcode},@s{stdout},@s{stderr},
              @f{rtime},@f{utime},@f{stime} FROM results WHERE id=%s"] id in
  let stdout = get_blob db (Misc.Blob.string out_id) in
  let stderr = get_blob db (Misc.Blob.string err_id) in
  let raw = { Event.errcode; stdout; stderr; rtime; utime; stime; } in
  let program = get_blob db (Misc.Blob.yojson Prover.of_yojson pv_id) in
  let problem = get_blob db (Misc.Blob.yojson Problem.of_yojson pb_id) in
  { Event.raw; program; problem; }

(* Assumes the problem and prover have already been added to the db. *)
let write_result db res =
  let id = Hash.pv_result res in
  if mem_result db id then
    () (* raise an error ? *)
  else begin
    let open Event in
    let status = Res.to_string @@ analyze_p res in
    let stdout_hash = Hash.string res.raw.stdout in
    let stderr_hash = Hash.string res.raw.stderr in
    let prover_hash = Hash.prover res.program in
    let problem_hash = Hash.problem res.problem in
    let () = write_blob db stdout_hash res.raw.stdout in
    let () = write_blob db stderr_hash res.raw.stderr in
    S.execute db [%sqlc
      "INSERT INTO results VALUES(%s,%s,%s,%s,%d,%s,%s,%f,%f,%f)"]
      id prover_hash problem_hash status res.raw.errcode
      stdout_hash stderr_hash res.raw.rtime res.raw.utime res.raw.stime
  end


(* The benchmarks table only stores the timestamp as metadata,
   the rest is stored in a blob, whose hash is stored. *)
let init_snapshots db =
  S.execute db [%sqlc
    "CREATE TABLE IF NOT EXISTS snapshots (
      uuid TEXT PRIMARY KEY ON CONFLICT ABORT,
      timestamp REAL,
      data TEXT
    )"]

let get_snapshot db id =
  let timestamp, meta =
    S.select_one db [%sqlc
      "SELECT @f{timestamp},@s{data} FROM snapshots WHERE uuid=%s"] id in
  let uuid = match Uuidm.of_string id with
    | Some x -> x
    | None ->
      assert false (* The id exists in the database, so it should be a legal uuid .*)
  in
  Snapshot.make ~uuid ~meta ~timestamp ()

let write_snapshot db s =
  let open Snapshot in
  S.execute db [%sqlc "INSERT INTO snapshots VALUES (%s,%f,%s)"]
    (Uuidm.to_string s.uuid) s.timestamp s.meta


(* The benchamrks<->results link table stores pairs of the benchamrk uuid and
   the results' hash. The primary key is put on the tuple of the two columns since
   we really want to store the set of such pairs. Additionally, we index both columns,
   since queries on them will be very common. *)
let init_snap_res db =
  let () = S.execute db [%sqlc
      "CREATE TABLE IF NOT EXISTS snap_res (
        snap TEXT,
        res TEXT,
        PRIMARY KEY (snap, res) ON CONFLICT IGNORE
      )"] in
  let () = S.execute db [%sqlc
      "CREATE INDEX IF NOT EXISTS bench_res_index_snap ON bench_res(snap)"] in
  let () = S.execute db [%sqlc
      "CREATE INDEX IF NOT EXISTS bench_res_index_res ON bench_res(res)"] in
  ()

let link_snap_res db snap res =
  S.execute db [%sqlc "INSERT INTO snap_res VALUES (%s,%s)"]
    (Uuidm.to_string snap.Snapshot.uuid) (Hash.pv_result res)

let snapshot_event_list db snap =
  S.select db [%sqlc
    "SELECT @s{res} FROM snap_res WHERE snap=%s"] (Uuidm.to_string snap.Snapshot.uuid)


(* The provers table store the name and version of the prover as metadat,
   the remaining information is stored as a blob whose hash is in the last column.
   An INDEX is ceated on name and version to speed up queries. *)
let init_provers db =
  let () = S.execute db [%sqlc
      "CREATE TABLE IF NOT EXISTS provers (
        id TEXT PRIMARY KEY ON CONFLICT IGNORE,
        name TEXT,
        version TEXT,
      )"] in
  let () = S.execute db [%sqlc
      "CREATE INDEX IF NOT EXISTS provers_index ON provers (name)"] in
  let () = S.execute db [%sqlc
      "CREATE INDEX IF NOT EXISTS provers_index ON provers (version)"] in
  ()

let write_prover db pv =
  let json = Prover.to_yojson pv in
  let id = Hash.yojson json in
  let () = write_json db id json in
  S.execute db [%sqlc "INSERT INTO provers VALUES (%s,%s,%s)"]
      id pv.Prover.name (version_to_string pv.Prover.version)

let get_prover db id =
  get_blob db (Misc.Blob.yojson Prover.of_yojson id)


(* The problems table stores the path and expected status of a problem.
   Again, indexed on the name and expected status. Problems contents
   are stored as blobs using the same id. *)
let init_problems db =
  let () = S.execute db [%sqlc
      "CREATE TABLE IF NOT EXISTS problems (
        id TEXT PRIMARY KEY ON CONFLICT IGNORE,
        path TEXT,
        expect TEXT
      )"] in
  let () = S.execute db [%sqlc
      "CREATE INDEX IF NOT EXISTS problems_index_path ON problems(path)"] in
  let () = S.execute db [%sqlc
      "CREATE INDEX IF NOT EXISTS problems_index_expect ON problems(expect)"] in
  ()

let write_problem db pb contents =
  let id = Hash.string contents in
  let () = write_blob db id contents in
  S.execute db [%sqlc "INSERT INTO problems VALUES(%s,%s,%s)"]
    id pb.Problem.name (Res.to_string pb.Problem.expected)

let get_problem db id =
  let (name, expected) = S.select_one db [%sqlc
      "SELECT (@s{path},@s{expect}) FROM problems WHERE id=%s"] id in
  Problem.make name (Res.of_string expected)

let get_problem_contents db id =
  get_blob db (Misc.Blob.string id)

(* Open the db and create tables if they don't exists.
   The stadard db file should be ~/.frogutils/frog.db *)
let make file =
  (* TODO: adjust options ?mutex and ?cache of open_db *)
  let db = S.open_db file in
  (* Initialize tables & indexs *)
  let () = init_blobs db in
  let () = init_results db in
  let () = init_snapshots db in
  let () = init_snap_res db in
  let () = init_provers db in
  let () = init_problems db in
  (* Finally return the initialised db. *)
  db


(* ---      Advanced queries    --- *)


(* ---     DB writer wrappers   --- *)


