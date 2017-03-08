
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Client-Side}

Most functions will use {!Lwt_log} as a side-effect. *)
open Frog

type remote_daemon

val connect : int -> (remote_daemon -> 'a Lwt.t) -> 'a Lwt.t
(** Connect the the remote daemon *)

val acquire : ?cwd:string -> ?user:string -> ?info:string ->
              ?cores:int -> ?priority: int -> ?tags:string list ->
              remote_daemon -> (bool -> 'a Lwt.t) -> 'a Lwt.t
(** [acquire daemon f] acquires the lock on the remote [daemon], then
    calls [f true] and returns the result of [f true].
    If the lock can't be acquired for some reason, [f false] is called instead.
    It always unlock the daemon after [f] returns.
    @param info additional info about the lock acquisition
    @param user who did ask for the lock? *)

val connect_or_spawn : ?retry:float -> int -> (remote_daemon -> 'a Lwt.t) -> 'a Lwt.t
(** Similar to {!connect}, but if it couldn't connect it launches the daemon
    and waits {!retry} seconds before retrying. *)

val get_status : int -> LockMessages.status_answer option Lwt.t
(** [get_status port] connects to the daemon on the given port (if any)
    and returns its status. It returns [None] if the daemon
    couldn't be contacted *)

val stop_accepting : int -> unit Lwt.t
(** Connect to daemon and tells it to stop accepting new jobs *)
