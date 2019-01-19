
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Storage of Events} *)

open Frog

type 'a or_error = 'a Misc.Err.t

let find_snapshot storage str : Event.Snapshot.t or_error Lwt.t =
  let open Misc.LwtErr in
  Storage.find_json storage str >>?=
  Event.Snapshot.of_yojson

let find_snapshot_exn storage str : Event.Snapshot.t Misc.LwtErr.t =
  let open Misc.LwtErr in
  Storage.find_json storage str >>?=
  Event.Snapshot.of_yojson

let list_snapshots storage : Event.Snapshot.t list Misc.LwtErr.t =
  let open Misc.LwtErr in
  let%lwt l = Storage.find_files storage in
  (* only keep names that are uuid *)
  let l = List.filter (fun file -> CCOpt.is_some (Uuidm.of_string file)) l in
  Misc.LwtErr.map_s
    (fun file -> Storage.find_json storage file >>?= Event.Snapshot.of_yojson)
    l

let find_meta storage (str:string) : Event.snapshot_meta or_error Lwt.t =
  let open Misc.LwtErr.Infix in
  Maki.mk1
    ~lifetime:Maki.(Lifetime.keep_for Time.(days 60))
    ~name:"event_storage.find_meta"
    Maki.Hash.string
    Run.maki_snapshot_meta
    ~f:(fun str ->
       find_snapshot_exn storage str >|= Event.meta)
    str

let list_meta storage : Event.snapshot_meta list Misc.LwtErr.t =
  let open Misc.LwtErr in
  let%lwt l = Storage.find_files storage in
  Misc.LwtErr.map_s (find_meta storage) l
