
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Daemon for Coordination} *)

open Frog

val default_port : int

val spawn : int -> unit Lwt.t
(** Spawn on the given port. Can fail. *)

val fork_daemon : int -> unit
(** Fork the process and spawn a daemon in the child
    (using {!spawn} in [frogdaemon]). *)

