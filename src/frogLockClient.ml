
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
  port : int;
}

let section = Logs.Src.create "FrogLockClient"

(* connect to daemon *)
let daemon_ch port f =
  Logs.debug ~src:section (fun k->k "trying to connect to daemon on port %d..." port);
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  CCUnix.with_connection addr ~f

let connect port f =
  daemon_ch port
    (fun _ oc ->
      Logs.debug ~src:section (fun k->k "connected to daemon");
      M.print oc M.Start;
      let res = f {port; } in
      M.print oc M.End;
      Logs.debug ~src:section (fun k->k "connection to daemon closed");
      res)

(* given the channels to the daemon, acquire lock, call [f], release lock *)
let acquire ?cwd ?user ?info ?(cores=0) ?(priority=1) ?(tags=[]) {port; } f =
  Logs.debug ~src:section (fun k->k "acquiring lock...");
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  CCUnix.with_connection addr
    ~f:(fun ic oc ->
       let query_time = Unix.gettimeofday() in
       (* send "acquire" *)
       let pid = Unix.getpid() in
       let msg = M.Acquire {M.info; user; priority; query_time; tags; cwd; pid; cores} in
       M.print oc msg;
       (* expect "go" *)
       let res = M.parse ic in
       match res with
       | M.Reject ->
         Logs.debug ~src:section (fun k->k "lock: rejected (daemon too busy?)");
         f false
       | M.Go ->
         Logs.debug ~src:section (fun k->k "acquired lock");
         let res = f true in
         Logs.debug ~src:section (fun k->k "release lock");
         M.print oc M.Release;
         res
       | _ ->
         Logs.err ~src:section (fun k->k "unexpected message: %a" M.pp res);
         raise (M.Unexpected res))

(* try to connect; if it fails, spawn daemon and retry *)
let connect_or_spawn ?log_file ?(retry=1) port f =
  try
    connect port f
  with _ ->
    (* launch daemon and re-connect *)
    Logs.info ~src:section (fun k->k "could not connect; launch daemon...");
    match FrogLockDaemon.fork_and_spawn ?log_file port with
    | `child thread ->
        Thread.join thread;
        raise Exit
    | `parent ->
        Unix.sleep retry;
        Logs.info ~src:section (fun k->k "retry to connect to daemon...");
        connect port f

let get_status port =
  try
    daemon_ch port
      (fun ic oc ->
        M.print oc M.Status;
        let res = M.parse ic in
        match res with
        | M.StatusAnswer ans -> Some ans
        | m -> raise (M.Unexpected m)
      )
  with _  -> None

(* connect to daemon (if any) and tell it to stop *)
let stop_accepting port =
  try
    daemon_ch port (fun _ oc -> M.print oc M.StopAccepting)
  with e ->
    Logs.err ~src:section (fun k->k "error: %s" (Printexc.to_string e));
    ()


