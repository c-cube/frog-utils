

(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Storage of Events} *)

open Frog

type 'a or_error = 'a Misc.Err.t

val list_snapshots : Storage.t -> Event.Snapshot.t list or_error Lwt.t
(** List and parse all snapshots from storage *)
