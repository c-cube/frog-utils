
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Storage on Disk}

    Maps keys, that are strings, to content *)

(* TODO: use inotify to watch the directories *)

open Frog

type t
(** The type of storage connection. *)

type id = string
(** Most data stored in the database is identified by its hash, encoded
    as a string. *)

val make : ?file:string -> unit -> t
(** [make ~file ()] opens the database in [file] and returns a storage conenction.
    (default file: ["~/.frogutils/frog.db"]) *)


(** {2 Database Getters} *)

(** {4 Basic getters} *)

val get_blob : t -> 'a Misc.Blob.t -> 'a
(** Get a stored typed blob.
    @raise Not_found if the blob doesn't exist in the database. *)

val get_snapshot  : t -> id -> Snapshot.t
val get_prover    : t -> id -> Prover.t
val get_problem   : t -> id -> Problem.t
val get_result    : t -> id -> Prover.t Event.result
(** Get stored data based on identifiers *)

val get_problem_contents : t -> id -> string
(** Returns the stored contents of a problem (i.e the contents of the problem file). *)

val snapshot_event_list : t -> Snapshot.t -> id list
(** Returns the list of ids of results associated with a snapshot. *)


(** {2 Writing to the database} *)



