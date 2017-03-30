
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Storage on Disk} *)

open Lwt.Infix
open Frog

type key = string
type json = Yojson.Safe.json
type 'a or_error = 'a Misc.Err.t

type t = {
  dirs: string list; (* absolute paths to directories *)
} [@@deriving yojson]

let make ?(conf=Config.empty) dirs : t =
  let f = Config.interpolate_home in
  let default =
    let s = f "$HOME/.frogutils/" in
    let ret = Sys.command ("mkdir -p " ^ s) in
    if ret=0 then (
      Lwt_log.ign_debug_f "default storage dir is `%s`" s;
      [s]
    )else []
  in
  let by_conf = Config.(get_or ~default:[] conf @@ string_list "storage") in
  let dirs = List.map f dirs @ List.map f by_conf @ default in
  { dirs }

module StrSet = Misc.StrSet

let find_files ?(filter=fun _ -> true) storage : key list Lwt.t =
  let aux acc d =
    let%lwt dir = Lwt_unix.opendir d in
    let acc = ref acc in
    try%lwt
      while%lwt true do
        let%lwt s = Lwt_unix.readdir dir in
        if s<>"." && s<>".." && filter s then (
          acc := StrSet.add s !acc;
        );
        Lwt.return_unit
      done >>= fun () ->
      assert false
    with End_of_file ->
      let%lwt () = Lwt_unix.closedir dir in
      Lwt.return !acc
  in
  Lwt_list.fold_left_s aux StrSet.empty storage.dirs
  >|= StrSet.elements

let save storage k v : unit Lwt.t = match storage.dirs with
  | [] -> assert false
  | dir :: _ ->
    let file = Filename.concat dir k in
    Lwt_io.with_file ~mode:Lwt_io.Output file
      (fun oc ->
         let%lwt () = Lwt_io.write oc v in
         Lwt_io.flush oc)

let save_json storage k v =
  save storage k (Yojson.Safe.to_string v)

let find_file storage (k:key): key or_error Lwt.t =
  let rec aux dirs = match dirs with
    | [] ->
      Lwt_log.ign_debug_f "storage: could not find key `%s` in directories %s"
        k
        (Misc.Fmt.to_string (Misc.Fmt.pp_list Format.pp_print_string) storage.dirs);
      Misc.LwtErr.fail "no directory for storage"
    | d :: dirs' ->
      let file = Filename.concat d k in
      if Sys.file_exists file
      then Misc.LwtErr.return file
      else aux dirs'
  in
  aux storage.dirs

let find storage (k:key) : string or_error Lwt.t =
  let open Misc.LwtErr in
  find_file storage k >>= fun filename ->
  (Misc_unix.File.with_in ~file:filename Misc_unix.File.read_all) |> ok

let delete storage (k:key): unit or_error Lwt.t =
  let open Misc.LwtErr in
  find_file storage k >>= fun filename ->
  (try Sys.remove filename; return ()
   with e -> fail (Printexc.to_string e))

let find_json storage k : json or_error Lwt.t =
  let open Misc.LwtErr in
  find storage k >>= fun s ->
  try
    Lwt.return (Yojson.Safe.from_string s |> Misc.Err.return)
  with e ->
    fail (Printexc.to_string e)


