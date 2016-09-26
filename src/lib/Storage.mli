
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Storage on Disk}

    Maps keys, that are {!Uuidm.t}, to content *)

(* TODO: use inotify to watch the directories *)

type key = string
type json = Yojson.Safe.json

type t = {
  dirs: string list; (* absolute paths to directories *)
}

val make : ?conf:Config.t -> string list -> t
(** [make dirs] creates a new storage on the given directories, from
    command line, config, etc. + PWD *)

val find_files : ?filter:(key -> bool) -> t -> key list Lwt.t
(** [iter storage] returns a list of files in the storage
    @param filter filter on files' unique ID *)

val save : t -> key -> string -> unit Lwt.t
(** [save storage key content] saves [content] under a file named
    after [key] *)

val save_json : t -> key -> json -> unit Lwt.t
(** [save storage key content] saves [content] under a file named
    after [key] *)

val find : t -> key -> string option Lwt.t
(** [find storage uuid] returns the content associated with the given key,
    or [None] *)

