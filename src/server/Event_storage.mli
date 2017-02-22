

(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Storage of Events} *)

open Frog

type 'a or_error = 'a Misc.Err.t

val find_snapshot : Storage.t -> string -> Event.Snapshot.t or_error Lwt.t
(** Fetch the snapshot by its name *)

val list_snapshots : Storage.t -> Event.Snapshot.t list or_error Lwt.t
(** List and parse all snapshots from storage *)

val find_meta: Storage.t -> string -> Event.snapshot_meta or_error Lwt.t

val list_meta : Storage.t -> Event.snapshot_meta list or_error Lwt.t
