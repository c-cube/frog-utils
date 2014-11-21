
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

let (>>=) = Lwt.(>>=)

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
let acquire ?cwd ?user ?info ?(tags=[]) {ic; oc} f =
  Lwt_log.ign_debug "acquiring lock...";
  let query_time = Unix.gettimeofday() in
  (* send "acquire" *)
  let pid = Unix.getpid() in
  let msg = M.Acquire {M.info; user; query_time; tags; cwd; pid} in
  M.print oc msg >>= fun () ->
  (* expect "go" *)
  M.parse ic >>= function
  | M.Reject ->
    Lwt_log.ign_debug ~section "lock: rejected (daemon too busy?)";
    f false
  | M.Go ->
    Lwt_log.ign_debug ~section "acquired lock";
    Lwt.finalize
      (fun () -> f true)
      (fun () ->
        (* eventually, release *)
        Lwt_log.ign_debug ~section "release lock";
        M.print oc M.Release
      )
  | msg -> Lwt.fail (M.Unexpected msg)

(* try to connect; if it fails, spawn daemon and retry *)
let connect_or_spawn ?log_file ?(retry=1.) port f =
  Lwt.catch
    (fun () -> connect port f)
    (fun _e ->
      (* launch daemon and re-connect *)
      Lwt_log.ign_info ~section "could not connect; launch daemon...";
      FrogLockDaemon.fork_and_spawn ?log_file port >>= function
      | `child thread ->
          thread >>= fun() ->
          Lwt.fail Exit
      | `parent ->
          Lwt_unix.sleep retry >>= fun () ->
          Lwt_log.ign_info ~section "retry to connect to daemon...";
          connect port f
    )

let get_status port =
  Lwt.catch
    (fun () ->
      connect port
        (fun daemon ->
          M.print daemon.oc M.Status >>= fun () ->
          M.parse daemon.ic >>= function
          | M.StatusAnswer ans ->
              Lwt.return (Some ans)
          | m ->
              Lwt.fail (M.Unexpected m)
        )
    ) (fun _ -> Lwt.return_none)

(* connect to daemon (if any) and tell it to stop *)
let stop_accepting port =
  Lwt.catch
    (fun () ->
      connect port
        (fun {oc;_} -> M.print oc M.StopAccepting)
    )
    (fun e ->
      Lwt_log.ign_error_f ~section "error: %s" (Printexc.to_string e);
      Lwt.return_unit
    )
