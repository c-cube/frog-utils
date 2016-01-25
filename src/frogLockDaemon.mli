
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Daemon} *)

val spawn : int -> unit
(** Spawn on the given port. Can fail. *)

val fork_and_spawn : ?log_file:string -> int ->
                      [> `child of Thread.t | `parent ]
(** Fork the process and spawn a daemon in the child (using {!spawn}).
    It returns [`child t] if the current process is the child, running
    [t] as the spawned server
    @param log_file path to the log file to use (None to disable) *)
