
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

(** {1 Client-Side} *)

module M = FrogLockMessages

type remote_daemon = {
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
}

let section = Lwt_log.Section.make "FrogLockClient"

let connect port f =
  Lwt_log.ign_debug_f ~section "trying to connect to daemon on port %d..." port;
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  Lwt_io.with_connection addr
    (fun (ic,oc) ->
      Lwt_log.ign_debug_f ~section "connected to daemon";
      f {ic; oc}
    )

(* given the channels to the daemon, acquire lock, call [f], release lock *)
let acquire ?cwd ?user ?info ?(priority=1) ?(tags=[]) {ic; oc} f =
  Lwt_log.ign_debug "acquiring lock...";
  let query_time = Unix.gettimeofday() in
  (* send "acquire" *)
  let pid = Unix.getpid() in
  let msg = M.Acquire {M.info; user; priority; query_time; tags; cwd; pid} in
  let%lwt () = M.print oc msg in
  (* expect "go" *)
  let%lwt res = M.parse ic in
  match res with
  | M.Reject ->
    Lwt_log.ign_debug ~section "lock: rejected (daemon too busy?)";
    f false
  | M.Go ->
    Lwt_log.ign_debug ~section "acquired lock";
    f true
    [@finally
      (* eventually, release *)
      Lwt_log.ign_debug ~section "release lock";
      M.print oc M.Release
    ]
  | msg -> Lwt.fail (M.Unexpected msg)

(* try to connect; if it fails, spawn daemon and retry *)
let connect_or_spawn ?log_file ?(retry=1.) port f =
  try%lwt
    connect port f
  with _ ->
    (* launch daemon and re-connect *)
    Lwt_log.ign_info ~section "could not connect; launch daemon...";
    match%lwt FrogLockDaemon.fork_and_spawn ?log_file port with
    | `child thread ->
        let%lwt () = thread in
        Lwt.fail Exit
    | `parent ->
        let%lwt () = Lwt_unix.sleep retry in
        Lwt_log.ign_info ~section "retry to connect to daemon...";
        connect port f

let get_status port =
  try%lwt
    connect port
      (fun daemon ->
        let%lwt () = M.print daemon.oc M.Status
        and res = M.parse daemon.ic in
        match res with
        | M.StatusAnswer ans ->
            Lwt.return (Some ans)
        | m ->
            Lwt.fail (M.Unexpected m)
      )
  with _  ->
    Lwt.return_none

(* connect to daemon (if any) and tell it to stop *)
let stop_accepting port =
  try%lwt
    connect port (fun {oc;_} -> M.print oc M.StopAccepting)
  with e ->
    Lwt_log.ign_error_f ~section "error: %s" (Printexc.to_string e);
    Lwt.return_unit
