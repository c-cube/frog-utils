
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Storage on Disk}

    Maps keys, that are strings, to content *)

(* TODO: use inotify to watch the directories *)

type key = string
type json = Yojson.Safe.json
type 'a or_error = 'a Misc.Err.t

type t = private {
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

val find : t -> key -> string or_error Lwt.t
(** [find storage uuid] returns the content associated with the given key,
    or [None] *)

val find_json : t -> key -> json or_error Lwt.t
(** [find storage uuid] returns the json content associated with the given key,
    or [None] *)
