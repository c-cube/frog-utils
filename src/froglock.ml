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

(** {1 Scheduling script} *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let section = Lwt_log.Section.make "FrogLock"

let () =
  FrogLockMessages.register_exn_printers();
  Lwt.async_exception_hook :=
    (fun e ->
      Lwt_log.ign_error_f "async error: %s" (Printexc.to_string e);
      exit 1)

let write_line oc line = Lwt_io.write_line oc line

type cmd =
  | Shell of string
  | Exec of string * string list
  | PrintStatus
  | StopAccepting
  [@@deriving show]

type parameters = {
  mails : string list;
  port : int;
  cmd : cmd;
  debug : bool;
  tags : string list; (* user-defined tags *)
}

(* result of running a command *)
type result = {
  res_cmd : string;
  out : string;
  time : float;  (* running time *)
  status : Unix.process_status;
  pid : int;
}

(* main task: acquire lock file, execute command [cmd], release lock *)
let run_command params =
  let info = show_cmd params.cmd in
  let user = try Some(Sys.getenv "USER") with _ -> None in
  let cwd = Sys.getcwd () in
  FrogLockClient.connect_or_spawn ~log_file:"/tmp/froglock.log" params.port
    (fun daemon ->
      FrogLockClient.acquire ~cwd ?user ~info ~tags:params.tags daemon
        (function
        | true ->
          let cmd, cmd_string = match params.cmd with
            | PrintStatus
            | StopAccepting -> assert false
            | Shell c -> Lwt_process.shell c, c
            | Exec (prog, args) ->
                let cmd = prog, Array.of_list (prog::args) in
                cmd, (String.concat " " (prog::args))
          in
          Lwt_log.ign_debug_f "start command %s" cmd_string;
          (* close stdin so that interactive commands fail *)
          Lwt_io.close Lwt_io.stdin >>= fun () ->
          let start = Unix.gettimeofday () in
          Lwt_process.with_process cmd
            (fun process ->
              (* launch command, read its output line by line and
                display it at the same time *)
              let lines_stream = Lwt_io.read_lines process#stdout in
              let buf = Buffer.create 256 in
              Lwt_stream.iter_s
                (fun line ->
                  Buffer.add_string buf line;
                  Buffer.add_char buf '\n';
                  Lwt_io.printl line
                ) lines_stream
              >>= fun () ->
              process#status >>= fun status ->
              (* measure time elapsed since we started the process *)
              let stop = Unix.gettimeofday () in
              let time = stop -. start in
              Lwt_log.ign_debug_f ~section "command finished after %.2fs" time;
              let res = {res_cmd=cmd_string; out=Buffer.contents buf;
                         time; status; pid=process#pid; } in
              Lwt.return (Some res)
            )
      | false ->
          Lwt_log.ign_info_f "could not acquire lock";
          Lwt.return_none
      )
    )

(* send a recap mail to the given address *)
let send_mail addr res =
  let real_addr = Smtp_lwt.Addr.of_string addr in
  let i = String.index addr '@' in
  let domain = String.sub addr (i+1) (String.length addr-i-1) in
  let from = "\"join-locke\" <joinlocke@example.com>" in
  Lwt_log.ign_debug_f "try to send a mail to %s..." addr;
  (* connect to SMTP server *)
  Smtp_lwt.connect ~host:domain ~name:"<joinlocke@example.com>" () >>= fun c ->
  let body = Printf.sprintf
    "Subject: job '%s' (%.2fs)\n\
    From: %s\n\
    To: %s \n\
    \n\
    %s" (String.escaped res.res_cmd) res.time from addr res.out in
  Smtp_lwt.send c
    ~from:(Smtp_lwt.Addr.of_string from)
    ~to_:[real_addr]
    ~body
  >>= function
  | `Ok (_,_) ->
      Lwt_log.ign_debug_f "succeeded in sending the mail to %s" addr;
      Lwt.return_unit
  | `Failure (c,msg) ->
      let msg = Printf.sprintf "could not send mail to %s: %s (code %d)" addr msg c in
      failwith msg

(* send mail to addresses *)
let send_mails params res =
  Lwt_list.iter_p
    (fun addr ->
      Lwt.catch
        (fun () -> send_mail addr res)
        (fun e ->
          let msg = Printexc.to_string e in
          Lwt_log.error_f "could not send mail to %s: %s" addr msg)
    ) params.mails

module M = FrogLockMessages

let maybe_str = function
  | None -> "<none>"
  | Some s -> s

(* connect to daemon (if any) and ask status *)
let print_status params =
  let tags2str tags = match tags with
    | [] -> ""
    | [t] -> ", tag " ^ t
    | l -> ", tags {" ^ String.concat ", " l ^ "}"
  in
  let now = Unix.gettimeofday() in
  Lwt.catch
    (fun () ->
      FrogLockClient.get_status params.port >>= function
      | None ->
        Lwt_log.info_f ~section "daemon not running"
      | Some {M.current=c; waiting} ->
          begin match c with
            | None -> Lwt.return_unit
            | Some c ->
                let time = Unix.gettimeofday() -. c.M.current_start in
                let job = c.M.current_job in
                Lwt_io.printlf "current job (user %s, pid %d, cwd %s, issued %.2fs ago%s, running for %.2fs): %s"
                  (maybe_str job.M.user)
                  job.M.pid (maybe_str job.M.cwd)
                  (now -. job.M.query_time)
                  (tags2str job.M.tags)
                  time (maybe_str job.M.info)
          end >>= fun () ->
          Lwt_list.iter_s
            (fun wjob ->
              let job = wjob.M.waiting_job in
              Lwt_io.printlf "waiting job n°%d (user %s, pid %d, cwd %s, issued %.2fs ago%s): %s"
                wjob.M.waiting_id
                (maybe_str job.M.user)
                job.M.pid (maybe_str job.M.cwd)
                (now -. job.M.query_time)
                (tags2str job.M.tags)
                (maybe_str job.M.info)
            ) waiting
    )
    (fun e ->
      Lwt_log.ign_error_f ~section "error: %s" (Printexc.to_string e);
      Lwt.return_unit
    )

let main params =
  Lwt_main.run
    (
      Lwt_log.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ();
      if params.debug then (
        Lwt_log.add_rule "*" Lwt_log.Debug
      );
      match params.cmd with
      | PrintStatus -> print_status params
      | StopAccepting -> FrogLockClient.stop_accepting params.port
      | Exec _
      | Shell _ ->
          run_command params >>= function
          | None -> Lwt.return_unit
          | Some res ->
            (* TODO: print more details, like return code *)
            Lwt_log.ign_info_f ~section "process ran in %.2fs (pid: %d)\n"
              res.time res.pid;
            send_mails params res
    )

(** {2 Main} *)

let port_ = ref 12000
let cmd_ = ref []
let debug_ = ref false
let shell_ = ref None
let mails_ = ref []
let status_ = ref false
let stop_accepting_ = ref false
let tags_ = ref []

let push_cmd_ s = cmd_ := s :: !cmd_
let set_shell_ s = shell_ := Some s
let add_mail_ s = mails_ := s :: !mails_
let add_tag_ s = tags_ := s :: !tags_

let options = Arg.align
  [ "-port", Arg.Set_int port_, " local port for the daemon"
  ; "-debug", Arg.Set debug_, " enable debug"
  ; "-mail", Arg.String add_mail_, " add mail address"
  ; "-c", Arg.String set_shell_, " use a shell command"
  ; "-tag", Arg.String add_tag_, " add a user-defined tag to a job"
  ; "-status", Arg.Set status_, " report status of the daemon (if any)"
  ; "-stop", Arg.Set stop_accepting_,
      " tell the daemon (if any) to stop accepting new jobs"
  ; "--", Arg.Rest push_cmd_, " start parsing command"
  ]

let usage_ = "lock [options] <cmd> <args>"

(* TODO: option to specify estimated completion time *)

let () =
  Arg.parse options push_cmd_ usage_;
  let mk_params cmd =
    { debug= !debug_; port= !port_; mails= !mails_; tags= !tags_; cmd; }
  in
  let params = match !shell_, List.rev !cmd_ with
    | _ when !status_ -> mk_params PrintStatus
    | _ when !stop_accepting_ -> mk_params StopAccepting
    | None, [] ->
        Arg.usage options usage_;
        exit 0
    | Some c, _ -> mk_params (Shell c)
    | None, head::args -> mk_params (Exec (head,args))
  in
  main params
