
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Client-Side}

Most functions will use {!Lwt_log} as a side-effect. *)
open Frog

module E = Misc.Err

type t
type msg = IPC_message.t

val connect : int -> (t -> 'a Lwt.t) -> 'a Lwt.t
(** Connect the the remote daemon
    @param port on which the daemon should be listening *)

val acquire : ?cwd:string -> ?user:string -> ?info:string ->
              ?cores:int -> ?priority: int -> ?tags:string list ->
              t -> (bool -> 'a Lwt.t) -> 'a Lwt.t
(** [acquire daemon f] acquires the lock on the remote [daemon], then
    calls [f true] and returns the result of [f true].
    If the lock can't be acquired for some reason, [f false] is called instead.
    It always unlock the daemon after [f] returns.
    @param info additional info about the lock acquisition
    @param user who did ask for the lock? *)

val connect_or_spawn : ?retry:float -> int -> (t -> 'a Lwt.t) -> 'a Lwt.t
(** Similar to {!connect}, but if it couldn't connect it launches the daemon
    and waits {!retry} seconds before retrying.
    @param port the port to connect to *)

val connect_and_acquire :
  ?cwd:string -> ?user:string -> ?info:string ->
  ?cores:int -> ?priority: int -> ?tags:string list ->
  ?retry:float ->
  int -> (t * bool -> 'a Lwt.t) -> 'a Lwt.t
(** Chain of {!connect_or_spawn} and {!acquire} *)

val send : t -> msg -> unit Lwt.t
(** Send message (broadcast) *)

val next_filter : t -> (msg -> bool) -> msg Lwt.t
(** Next incoming message matching the predicate *)

val next_filter_map : t -> (msg -> 'a option) -> 'a Lwt.t
(** Next incoming message that returns [Some x] *)

val on_msg : t -> (msg -> [`Continue | `Stop]) -> unit
(** [loop_msg t f] calls [f] on every incoming message.
    If [f msg] returns [`Stop], then [f] will not be called any more *)

val get_status : t -> IPC_message.status_answer Lwt.t
(** [get_status c] returns the current status of the daemon. *)

val connect_and_get_status : int -> IPC_message.status_answer option Lwt.t
(** [get_status port] connects to the daemon on the given port (if any)
    and returns its status. It returns [None] if the daemon
    couldn't be contacted *)

val stop_accepting_jobs : int -> unit Lwt.t
(** Connect to daemon and tells it to stop accepting new froglock jobs *)

val wait_close : t -> unit Lwt.t
(** Wait for the connection to be closed *)
