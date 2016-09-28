
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Result

module StrMap = Misc.StrMap

[@@@warning "-39"]
type t = {
  name: string;  (* filename *)
  expected: Res.t; (* result expected *)
} [@@deriving yojson]
[@@@warning "+39"]

type problem = t

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
  let%lwt content = Misc.File.with_in ~file Misc.File.read_all in
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
  | Res.Timeout, Res.Unknown
  | Res.Unknown, Res.Timeout
  | (Res.Sat | Res.Unsat | Res.Error), (Res.Unknown | Res.Timeout) -> `Disappoint
  | (Res.Unsat | Res.Error), Res.Sat
  | (Res.Sat | Res.Error), Res.Unsat
  | (Res.Sat | Res.Unknown | Res.Timeout | Res.Unsat), Res.Error ->
    `Mismatch
  | (Res.Unknown | Res.Timeout), (Res.Sat | Res.Unsat) ->
    `Improvement

let print out p =
  Format.fprintf out "@[<h>%s (expect: %a)@]" p.name Res.print p.expected

let to_string p = Misc.Fmt.to_string print p

let maki =
  let module V = Maki.Value in
  V.map ~descr:"problem"
    (fun p -> p.name, p.expected)
    (fun (name, expected) -> {name;expected})
    (V.pair V.file Res.maki)

(* Hashes of problems *)
let hash p : string =
  Sha1.string p.name |> Sha1.to_hex

(* HTML server *)
let to_html_name p = Html.div [Html.pcdata (Filename.basename p.name)]

let uri_of_problem pb =
  Uri.make ~path:"/problem/" ~query:["file", [pb.name]] ()

module Tbl = struct
  type t = problem StrMap.t

  let empty = StrMap.empty
  let add pb t = StrMap.add pb.name pb t
  let add_l = List.fold_right add
  let find_by_name n t = try Some (StrMap.find n t) with Not_found -> None
  let to_list t = StrMap.fold (fun _ pb acc->pb::acc) t []
end
