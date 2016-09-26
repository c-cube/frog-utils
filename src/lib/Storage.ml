
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Storage on Disk} *)

open Lwt.Infix

type key = string
type json = Yojson.Safe.json

type t = {
  dirs: string list; (* absolute paths to directories *)
}

let make ?(conf=Config.empty) dirs : t =
  let default =
    let s = Config.interpolate_home "$HOME/.frogutils/" in
    let ret = Sys.command ("mkdir -p " ^ s) in
    if ret=0 then [s] else []
  in
  let by_conf = Config.get_string_list ~default:[] conf "storage" in
  let dirs = dirs @ by_conf @ default in
  { dirs }

let find_files ?(filter=fun _ -> true) storage : string list Lwt.t =
  let aux d =
    let%lwt dir = Lwt_unix.opendir d in
    let acc = ref [] in
    try%lwt
      while%lwt true do
        let%lwt s = Lwt_unix.readdir dir in
        if filter s then acc := s :: !acc;
        Lwt.return_unit
      done >>= fun () ->
      assert false
    with End_of_file ->
      let%lwt () = Lwt_unix.closedir dir in
      Lwt.return !acc
  in
  Lwt_list.map_p aux storage.dirs >|= List.flatten

let save storage k v : unit Lwt.t = match storage.dirs with
  | [] -> assert false
  | dir :: _ ->
    let file = Filename.concat dir k in
    Lwt_io.with_file ~mode:Lwt_io.Output file
      (fun oc -> Lwt_io.write oc v)

let save_json storage k v =
  save storage k (Yojson.Safe.to_string v)

let find storage (k:key) : string option Lwt.t =
  let rec aux dirs = match dirs with
    | [] -> Lwt.return_none
    | d :: dirs' ->
      let file = Filename.concat d k in
      if Sys.file_exists file
      then
        Misc.File.with_in ~file Misc.File.read_all >|= Misc.Opt.return
      else aux dirs'
  in
  aux storage.dirs


