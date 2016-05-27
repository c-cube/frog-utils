
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

module MStr = Map.Make(String)

let fpf = Format.fprintf

type raw_result = {
  prover : Prover.t;
  problem: Problem.t;
  res: Res.t;
  stdout: string;
  stderr: string;
  errcode: int;
  rtime : (float [@default 0.]);
  utime : (float [@default 0.]);
  stime : (float [@default 0.]);
} [@@deriving yojson {strict=false}]

type raw = raw_result MStr.t

let raw_of_list l =
  List.fold_left
    (fun acc r -> MStr.add r.problem.Problem.name r acc)
    MStr.empty l

let raw_to_yojson r : Yojson.Safe.json =
  let l = MStr.fold
      (fun _ r acc ->
         let j = raw_result_to_yojson r in
         j :: acc)
      r []
  in
  `List l

let assoc_or def x l =
  try List.assoc x l
  with Not_found -> def

let raw_of_yojson (j:Yojson.Safe.json) =
  let open E in
  let get_list_ = function
    | `List l -> return l
    | _ -> fail "expected list"
  in
  get_list_ j
  >|= List.map raw_result_of_yojson
  >|= List.map of_err
  >>= seq_list
  >|= raw_of_list
  |> to_err

type stat = {
  unsat: int;
  sat: int;
  errors: int;
  unknown: int;
}

let stat_empty = {unsat=0; sat=0; errors=0; unknown=0; }

let add_sat_ s = {s with sat=s.sat+1}
let add_unsat_ s = {s with unsat=s.unsat+1}
let add_unknown_ s = {s with unknown=s.unknown+1}
let add_error_ s = {s with errors=s.errors+1}

let pp_stat out s =
  fpf out "{@[<hv>unsat: %d,@ sat: %d,@ errors: %d,@ unknown: %d,@ total: %d@]}"
    s.unsat s.sat s.errors s.unknown (s.unsat + s.sat + s.errors + s.unknown)

type t = {
  raw: raw;
  stat: stat;
  improved: raw_result list;
  ok: raw_result list;
  disappoint: raw_result list;
  bad: raw_result list;
}

let pp_raw_res_ out r =
  fpf out "@[<h>problem %s (expected: %a, result: %a)@]"
    r.problem.Problem.name Res.print r.problem.Problem.expected Res.print r.res

let pp_list_ p = Format.pp_print_list p

(* build statistics and list of mismatch from raw results *)
let analyse_ raw =
  let module M = OLinq.AdaptMap(MStr) in
  let l =
    M.of_map raw
    |> OLinq.map snd
    |> OLinq.group_by
      (fun r -> Problem.compare_res r.problem r.res)
    |> OLinq.run_list ?limit:None
  in
  let improved = assoc_or [] `Improvement l in
  let ok = assoc_or [] `Same l in
  let bad = assoc_or [] `Mismatch l in
  let disappoint = assoc_or [] `Disappoint l in
  (* stats *)
  let stat = ref stat_empty in
  let add_res res =
    stat := (match res with
        | Res.Unsat -> add_unsat_ | Res.Sat -> add_sat_
        | Res.Unknown -> add_unknown_ | Res.Error -> add_error_
      ) !stat
  in
  MStr.iter (fun _ r -> add_res r.res) raw;
  improved, ok, bad, disappoint, !stat

let add_raw raw r =
  MStr.add r.problem.Problem.name r raw

let make raw =
  let improved, ok, bad, disappoint, stat = analyse_ raw in
  { raw; stat; improved; ok; disappoint; bad; }

let of_yojson j = match raw_of_yojson j with
  | `Ok x -> `Ok (make x)
  | `Error s -> `Error s
let to_yojson t = raw_to_yojson t.raw

let of_list l =
  let raw = raw_of_list l in
  make raw

let of_file ~file =
  try
    let json = Yojson.Safe.from_file file in
    of_yojson json |> E.of_err
  with e -> E.fail (Printexc.to_string e)

let to_file t ~file = Yojson.Safe.to_file file (to_yojson t)

let is_ok r = r.bad = []
let num_failed r = List.length r.bad

let print out r =
  let pp_l out = fpf out "[@[<hv>%a@]]" (pp_list_ pp_raw_res_) in
  fpf out
    "@[<hv2>results: {@,stat:%a,@ %-15s: %a,@ %-15s: %a,@ %-15s: %a@]@,}"
    pp_stat r.stat
    "ok" pp_l r.ok
    "disappoint" pp_l r.disappoint
    "bad" pp_l r.bad

let maki_raw_res =
  Maki_yojson.make_err "raw_res"
    ~to_yojson:raw_result_to_yojson
    ~of_yojson:raw_result_of_yojson

(* FIXME: use result, not poly variant *)
let maki =
  Maki_yojson.make_err "results"
    ~to_yojson
    ~of_yojson

(* DB management *)
let db_init t =
  Sqlexpr.execute t [%sql
    "CREATE TABLE IF NOT EXISTS results (
      prover STRING, problem STRING, res STRING,
      stdout STRING, stderr STRING, errcode INTEGER,
      rtime REAL, utime REAL, stime REAL)"]

let import db (prover_hash, problem_hash, res_s,
               stdout, stderr, errcode, rtime, utime, stime) =
  match FrogProver.find db prover_hash with
  | Some prover ->
    begin match FrogProblem.find db problem_hash with
      | Some problem ->
        let res = FrogRes.of_string res_s in
        Some { prover; problem; res; stdout; stderr;
               errcode; rtime; utime; stime; }
      | None -> None
    end
  | None -> None

let find db prover problem =
  match Sqlexpr.select_one_maybe db [%sqlc
          "SELECT @s{prover}, @s{problem}, @s{res},
                  @s{stdout}, @s{stderr}, @d{errcode},
                  @f{rtime}, @f{utime}, @f{stime}
            FROM results
            WHERE prover=%s AND problem=%s"] prover problem with
  | Some x -> import db x
  | None -> None

let db_add db t =
  let () = FrogProver.db_add db t.prover in
  let () = FrogProblem.db_add db t.problem in
  let prover_hash = FrogProver.hash t.prover in
  let problem_hash = FrogProblem.hash t.problem in
  Sqlexpr.execute db [%sql "INSERT INTO results VALUES (%s,%s,%s,%s,%s,%d,%f,%f,%f)"]
    prover_hash problem_hash (FrogRes.to_string t.res)
    t.stdout t.stderr t.errcode t.rtime t.utime t.stime

(* display the raw result *)
let to_html_raw_result_name uri_of_raw r =
  H.a ~href:(uri_of_raw r) (H.string (FrogRes.to_string r.res))

let to_html_raw_result uri_of_problem r =
  R.start
  |> R.add "problem"
    (H.a ~href:(uri_of_problem r.problem) (Problem.to_html_name r.problem))
  |> R.add "result" (Res.to_html r.res)
  |> R.add_int "errcode" r.errcode
  |> R.add "stdout" (W.pre (H.string r.stdout))
  |> R.add "stderr" (W.pre (H.string r.stderr))
  |> R.close

let to_html_stats s =
  R.start
  |> R.add_int "unsat" s.unsat
  |> R.add_int "sat" s.sat
  |> R.add_int "errors" s.errors
  |> R.add_int "unknown" s.unknown
  |> R.close

let to_html_raw_result_l uri_of_problem uri_of_raw_res r =
  [ H.a ~href:(uri_of_problem r.problem) (Problem.to_html_name r.problem)
  ; H.a ~href:(uri_of_raw_res r) (Res.to_html r.res)
  ]

let to_html_raw uri_of_problem uri_of_raw_res r =
  let l = MStr.fold (fun _ r acc -> `Row r::acc) r [] in
  if l = [] then H.string "ø"
  else H.Create.table ~flags:[H.Create.Tags.Headings_fst_row]
      ~row:(function
          | `Head -> [H.string "problem"; H.string "result"]
          | `Row r -> to_html_raw_result_l uri_of_problem uri_of_raw_res r)
      (`Head :: l)

let to_html_summary t =
  H.Create.table
    ~flags:[H.Create.Tags.Headings_fst_col]
    ~row:(fun (s, i) -> [H.string s; H.int i])
    [
      "ok", (List.length t.ok);
      "improved", (List.length t.improved);
      "disappoint", (List.length t.disappoint);
      "bad", (List.length t.bad);
      "total", (MStr.cardinal t.raw);
    ]

let to_html uri_of_problem uri_of_raw_res t =
  let lst_raw_res ?cls l =
    if l=[] then H.string "ø"
    else H.Create.table l ~flags:[]
        ~row:(to_html_raw_result_l uri_of_problem uri_of_raw_res)
         |> H.div ?cls
  in
  R.start
  |> R.add "summary" (to_html_summary t)
  |> R.add "improved" (lst_raw_res t.improved)
  |> R.add "ok" (lst_raw_res t.ok)
  |> R.add "disappoint" (lst_raw_res t.disappoint)
  |> R.add "bad" (lst_raw_res t.bad)
  |> R.add "stats" (to_html_stats t.stat)
  |> R.add "raw" (to_html_raw uri_of_problem uri_of_raw_res t.raw)
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
          H.string "Problems" :: (List.map FrogProver.to_html_name provers)
        | `Pb pb ->
          FrogProblem.to_html_name pb :: List.map (
            fun prover ->
              match find db (FrogProver.hash prover) (FrogProblem.hash pb) with
              | Some raw -> to_html_raw_result_name uri_of_raw raw
              | None -> H.string "no result"
          ) provers
      )

let k_add = W.HMap.Key.create ("add_result", fun _ -> Sexplib.Sexp.Atom "")
let k_set = W.HMap.Key.create ("set_results", fun _ -> Sexplib.Sexp.Atom "")

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
        W.Server.return_html (to_html_raw_result uri_of_problem res)
      | None ->
        let code = Cohttp.Code.status_of_code 404 in
        let h = H.string ("could not find result") in
        W.Server.return_html ~code h
    end
  (* display all the results *)
  and handle_main _ =
    let uri_of_raw_res r = Uri.make
        ~path:(Printf.sprintf "/raw/%s/%s"
                 (FrogProver.hash r.prover) (FrogProblem.hash r.problem)) () in
    let h = to_html_db uri_of_prover uri_of_problem uri_of_raw_res (W.Server.db s) in
    W.Server.return_html ~title:"results" h
  and on_add r = db_add (W.Server.db s) r in
  W.Server.set s k_add on_add;
  W.Server.add_route s ~descr:"current results" "/results" handle_main;
  W.Server.add_route s "/raw/:prover/:problem" handle_res;
  ()

