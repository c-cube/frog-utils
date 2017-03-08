
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Daemon} *)

open Frog

val default_port : int

val spawn : int -> unit Lwt.t
(** Spawn on the given port. Can fail. *)

val fork_and_spawn : int -> [> `child of unit Lwt.t | `parent ] Lwt.t
(** Fork the process and spawn a daemon in the child (using {!spawn}).
    It returns [`child t] if the current process is the child, running
    [t] as the spawned server. *)

