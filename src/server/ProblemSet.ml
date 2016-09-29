
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Frog
open Result

type t = Problem.problem_set

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
  let%lwt content = Misc_unix.File.with_in ~file Misc_unix.File.read_all in
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

let make_pb ~default_expect ~file () =
  try%lwt
    Lwt_log.ign_debug_f "convert `%s` into problem..." file;
    let%lwt res = find_expected_ ~default_expect ~file () in
    let pb = Problem.make file res in
    Lwt.return (Ok pb)
  with e ->
    Lwt.return
      (Error (
          Format.asprintf "could not find expected res for %s: %s"
            file (Printexc.to_string e)))

let make ~default_expect l =
  let pool = Lwt_pool.create 30 (fun () -> Lwt.return_unit) in
  let%lwt l =
    Lwt_list.map_p
      (fun file ->
         Lwt_pool.use pool
           (fun () -> make_pb ~default_expect ~file ()))
      l
  in
  let l = Misc.Err.seq_list l in
  (* sort by alphabetic order *)
  let l = Misc.Err.(l >|= List.sort Problem.compare_name) in
  Lwt.return l

let size = List.length

let of_dir ~default_expect ?filter:(p=fun _ -> true) d =
  let l = Misc_unix.File.walk d in
  let l = Misc.List.filter_map
      (fun (kind,f) -> match kind with
         | `File when p f -> Some f
         | _ -> None
      ) l
  in
  make ~default_expect l

let print out set =
  Format.fprintf out "@[<hv>%a@]" (Format.pp_print_list Problem.print) set

