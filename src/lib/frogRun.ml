
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Result
open FrogDB

module W = FrogWeb
module H = W.Html
module R = W.Record
module E = FrogMisc.Err

module Res = FrogRes
module Prover = FrogProver
module Problem = FrogProblem

let fpf = Format.fprintf

type raw_result = {
  (* Raw output *)
  errcode: int;
  stdout: string;
  stderr: string;

  (* Time used *)
  rtime : float;
  utime : float;
  stime : float;
} [@@deriving yojson]

type prover  = [ `Prover of Prover.t ]  [@@deriving yojson]
type checker = [ `Checker of unit ]     [@@deriving yojson]
type program = [ prover | checker ]     [@@deriving yojson]

type +'a result = {
  program : 'a;
  problem : Problem.t;
  raw : raw_result;
} constraint 'a = [< program ]
    [@@deriving yojson]

let hash_prog t =
  match t.program with
  | `Prover prover -> FrogProver.hash prover
  | `Checker checker -> "TODO"

let maki_raw_res =
  let of_yojson x = E.to_exn (raw_result_of_yojson x) in
  Maki_yojson.make "raw_res"
    ~to_yojson:raw_result_to_yojson
    ~of_yojson

let (maki_result : _ result Maki.Value.ops) =
  let of_yojson x = E.to_exn (result_of_yojson program_of_yojson x) in
  Maki_yojson.make "result"
    ~to_yojson:(result_to_yojson program_to_yojson)
    ~of_yojson

(* Start processes *)
type env = (string * string) array

let analyze_p t =
  let `Prover prover = t.program in
  (* find if [re: re option] is present in [stdout] *)
  let find_opt_ re = match re with
    | None -> false
    | Some re ->
      Re.execp (Re_posix.compile_pat re) t.raw.stdout ||
      Re.execp (Re_posix.compile_pat re) t.raw.stderr
  in
  if t.raw.errcode = 0 then
    if find_opt_ prover.Prover.sat then Res.Sat
    else if find_opt_ prover.Prover.unsat then Res.Unsat
    else Res.Unknown
  else if find_opt_ prover.Prover.timeout then Res.Timeout
  else if find_opt_ prover.Prover.unknown then Res.Unknown
  else Res.Error

let mk_cmd ?env ~timeout ~memory ~prover ~file () =
  Lwt_log.ign_debug_f "timeout: %d, memory: %d" timeout memory;
  let cmd = FrogProver.make_command ?env prover ~timeout ~memory ~file in
  let cmd = ["/bin/sh"; "-c"; cmd] in
  "/bin/sh", Array.of_list cmd

let run_proc ~timeout cmd =
  let start = Unix.gettimeofday () in
  (* slightly extended timeout *)
  Lwt_process.with_process_full cmd
    (fun p ->
       let killed = ref false in
       (* Setup alarm to enforce timeout *)
       let pid = p#pid in
       let al = Lwt_timeout.create (timeout + 1)
           (fun () -> killed := true; Unix.kill pid Sys.sigkill) in
       Lwt_timeout.start al;
       (* Convenience function to get error code *)
       let res =
         let res_errcode =
           Lwt.map
             (function
               | Unix.WEXITED e
               | Unix.WSIGNALED e
               | Unix.WSTOPPED e -> e)
             p#status
         in
         (* Wait for the process to end, then get all output *)
         try%lwt
           let%lwt () = Lwt_io.close p#stdin in
           let out = Lwt_io.read p#stdout in
           let err = Lwt_io.read p#stderr in
           let%lwt code = res_errcode in
           let errcode = if !killed then 0 else code in
           Lwt_log.ign_debug_f "errcode: %d\n" errcode;
           Lwt_timeout.stop al;
           (* Compute time used by the prover *)
           let rtime = Unix.gettimeofday () -. start in
           let%lwt rusage = p#rusage in
           let utime = rusage.Lwt_unix.ru_utime in
           let stime = rusage.Lwt_unix.ru_stime in
           (* now finish reading *)
           Lwt_log.ign_debug_f "reading...\n";
           let%lwt stdout = out in
           let%lwt stderr = err in
           Lwt_log.ign_debug_f "closing...\n";
           let%lwt _ = p#close in
           Lwt_log.ign_debug_f "done closing & reading, return\n";
           Lwt.return {
             stdout; stderr; errcode;
             rtime; utime; stime; }
         with e ->
           Lwt_timeout.stop al;
           let%lwt code = res_errcode in
           let errcode = if !killed then 0 else code in
           let rtime = Unix.gettimeofday () -. start in
           let stderr = Printexc.to_string e in
           Lwt_log.ign_debug_f ~exn:e "error while running %s"
             ([%show: (string * string array)] cmd);
           Lwt.return {
             stdout = ""; stderr; errcode;
             rtime; utime = 0.; stime = 0.; }
       in
       res
    )

let run_prover ?env ~timeout ~memory ~prover ~pb () =
  let file = pb.FrogProblem.name in
  let cmd = mk_cmd ?env ~timeout ~memory ~prover ~file () in
  let%lwt raw = run_proc ~timeout cmd in
  Lwt.return {
    program = `Prover prover;
    problem = pb; raw; }


(* DB management *)
let db_init t =
  FrogDB.exec t
    "CREATE TABLE IF NOT EXISTS results (
      program STRING, problem STRING,
      stdout STRING, stderr STRING, errcode INTEGER,
      rtime REAL, utime REAL, stime REAL,
      PRIMARY KEY (program, problem) ON CONFLICT REPLACE)"
    (fun _ -> ())

let import db (r:FrogDB.row) =
  let module D = FrogDB.D in
  match r with
  | [| D.BLOB program_hash
     ; D.BLOB problem_hash
     ; D.BLOB stdout
     ; D.BLOB stderr
     ; D.INT errcode
     ; D.FLOAT rtime
     ; D.FLOAT utime
     ; D.FLOAT stime
    |] ->
    let errcode = Int64.to_int errcode in
    let raw = { stdout; stderr; errcode; rtime; utime; stime; } in
    begin match FrogProblem.find db problem_hash with
      | Some problem ->
        begin match FrogProver.find db program_hash with
          | Some prover ->
            Some { program = `Prover prover; problem; raw; }
          | None -> None
        end
      | None -> None
      (* Check the checker... *)
    end
  | _ -> assert false

let find db program problem =
  FrogDB.exec_a db
    "SELECT program, problem,
            stdout, stderr, errcode,
            rtime, utime, stime
      FROM results
      WHERE program=? AND problem=?"
    [| FrogDB.D.string program; FrogDB.D.string problem |]
    (fun c -> match FrogDB.Cursor.head c with
       | Some r -> import db r
       | None -> None)

let db_add db t =
  let module D = FrogDB.D in
  let program_hash = match t.program with
    | `Prover prover ->
      FrogProver.db_add db prover;
      FrogProver.hash prover
    | `Checker () ->
      ""
  in
  let () = FrogProblem.db_add db t.problem in
  let problem_hash = FrogProblem.hash t.problem in
  FrogDB.exec_a db
    "INSERT INTO results VALUES (?,?,?,?,?,?,?,?);"
    [| D.BLOB program_hash
     ; D.BLOB problem_hash
     ; D.BLOB t.raw.stdout
     ; D.BLOB t.raw.stderr
     ; D.int t.raw.errcode
     ; D.FLOAT t.raw.rtime
     ; D.FLOAT t.raw.utime
     ; D.FLOAT t.raw.stime
    |]
    (fun _ -> ())

(* Display a result's analyzed result *)
let to_html_analyzed = function
  | { program = `Prover prover; _ } as t ->
    let res = analyze_p t in
    FrogRes.to_html res
  | { program = `Checker checker; _ } -> H.string "TODO"

(* display the raw result *)
let to_html_raw_result_name uri_of_raw (r: program result) =
  let content = to_html_analyzed r in
  H.a ~href:(uri_of_raw r) content

let to_html_raw_result uri_of_prover uri_of_problem r =
  R.start
  |> R.add "program" (
    match r with
    | { program = `Prover prover; _ } ->
      (H.a ~href:(uri_of_prover prover) (FrogProver.to_html_name prover))
    | { program = `Checker checker; _ } ->
      H.string "TODO"
  )
  |> R.add "problem"
    (H.a ~href:(uri_of_problem r.problem) (Problem.to_html_name r.problem))
  |> R.add "result" (to_html_analyzed r)
  |> R.add_int "errcode" r.raw.errcode
  |> R.add_string "real time" (Printf.sprintf "%.3f" r.raw.rtime)
  |> R.add_string "user time" (Printf.sprintf "%.3f" r.raw.utime)
  |> R.add_string "system time" (Printf.sprintf "%.3f" r.raw.stime)
  |> R.add "stdout" (W.pre (H.string r.raw.stdout))
  |> R.add "stderr" (W.pre (H.string r.raw.stderr))
  |> R.close

let to_html_db uri_of_prover uri_of_pb uri_of_raw db =
  let pbs = List.map (fun x -> `Pb x) @@ List.sort
      (fun p p' -> compare p.FrogProblem.name p'.FrogProblem.name)
      (FrogProblem.find_all db) in
  let provers = List.sort
      (fun p p' -> compare p.FrogProver.binary p'.FrogProver.binary)
      (FrogProver.find_all db) in
  H.Create.table (`Head :: pbs) ~flags:[H.Create.Tags.Headings_fst_row]
    ~row:(function
        | `Head ->
          H.string "Problem" ::
          H.string "Expected" :: (
            List.map (fun x ->
                H.a ~href:(uri_of_prover x) (FrogProver.to_html_fullname x)
              ) provers)
        | `Pb pb ->
          H.a ~href:(uri_of_pb pb) (FrogProblem.to_html_name pb) ::
          FrogRes.to_html pb.FrogProblem.expected ::
          List.map (
            fun prover ->
              match find db (FrogProver.hash prover) (FrogProblem.hash pb) with
              | Some raw -> to_html_raw_result_name uri_of_raw raw
              | None -> H.string "<none>"
          ) provers
      )

let k_add = W.HMap.Key.create ("add_result", fun _ -> Sexplib.Sexp.Atom "")

let add_server s =
  let open Opium.Std in
  let uri_of_problem = W.Server.get s Problem.k_uri in
  let uri_of_prover = W.Server.get s Prover.k_uri in
  (* find raw result *)
  let handle_res req =
    let prover = param req "prover" in
    let pb = param req "problem" in
    begin match find (W.Server.db s) prover pb with
      | Some res ->
        W.Server.return_html
          (W.Html.list [
              W.Html.h2 (W.Html.string "Raw Result");
              to_html_raw_result uri_of_prover uri_of_problem res;
            ])
      | None ->
        let code = Cohttp.Code.status_of_code 404 in
        let h = H.string ("could not find result") in
        W.Server.return_html ~code h
    end
  (* display all the results *)
  and handle_main _ =
    let uri_of_raw_res r =
      Uri.make ~path:(
        Printf.sprintf "/raw/%s/%s"
          (hash_prog r)
          (FrogProblem.hash r.problem)
      ) () in
    let h = to_html_db uri_of_prover uri_of_problem uri_of_raw_res (W.Server.db s) in
    W.Server.return_html ~title:"Results"
      (W.Html.list [ W.Html.h2 (W.Html.string "Results"); h])
  and on_add r = db_add (W.Server.db s) r in
  W.Server.set s k_add on_add;
  W.Server.add_route s ~descr:"current results" "/results" handle_main;
  W.Server.add_route s "/raw/:prover/:problem" handle_res;
  ()

