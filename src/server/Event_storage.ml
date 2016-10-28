
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Storage of Events} *)

open Frog

type 'a or_error = 'a Misc.Err.t

let list_snapshots storage =
  let open Misc.LwtErr in
  let%lwt l = Storage.find_files storage in
  Misc.LwtErr.map_s
    (fun file -> Storage.find_json storage file >>?= Event.Snapshot.of_yojson)
    l
