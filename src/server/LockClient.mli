
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

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
