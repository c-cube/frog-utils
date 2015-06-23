
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

(** {1 Communication Protocole} *)

[@@@warning "-39"]

(** Description of a froglock job.

    Some fields have default values for backward compatibility (old client, new server);
    The flag [{strict=false}] ensures backward compatibility (new client, old server).
*)
type job = {
  info        : string option [@key "info"];
  user        : string option [@key "user"];
  priority    : int [@default 10] [@key "priority"];
  query_time  : float [@key "query_time"]; (* time at which the query was issued *)
  cwd         : string option [@key "cwd"]; (* working dir *)
  tags        : string list [@key "tags"];
  pid         : int [@key "pid"];
  cores       : int [@default 0] [@key "cores"];
}
[@@deriving yojson {strict=false},show]

type current_job = {
  current_id    : int [@key "id"];
  current_job   : job [@key "job"];
  current_start : float [@key "start"];  (* time at which task started *)
} [@@deriving yojson,show]

type waiting_job = {
  waiting_id    : int [@key "id"];
  waiting_job   : job [@key "job"];
} [@@deriving yojson, show]

type status_answer = {
  max_cores : int [@key "cores"];
  current : current_job list [@key "current"];
  waiting : waiting_job list [@key "waiting"];
} [@@deriving yojson,show]

type t =
  | Start [@name "start"]
  | End [@name "end"]
  | Acquire of job [@name "acquire"]
  | Go [@name "go"]
  | Release [@name "release"]
  | Reject [@name "reject"]
  | Status [@name "status"]
  | StatusAnswer  of status_answer [@name "statusanswer"]
  | StopAccepting [@name "stopaccepting"] (* from now on, no more accepts *)
  [@@deriving yojson, show]

[@@@warning "+39"]

exception InvalidMessage of string
exception Unexpected of t

let register_exn_printers () =
  Printexc.register_printer
   (function
      | Unexpected t -> Some ("unexpected message " ^ show t)
      | _ -> None);
  Printexc.register_printer
    (function
      | (InvalidMessage s) -> Some ("invalid message: " ^ s)
      | _ -> None);
  ()

let expect ic p =
  let%lwt s = Lwt_io.read_line ic in
  let%lwt res = Lwt.wrap (fun () -> Yojson.Safe.from_string s) in
  match of_yojson res with
  | `Ok m when p m -> Lwt.return m
  | `Ok _ -> Lwt.fail (InvalidMessage ("unexpected " ^ s))
  | `Error msg -> Lwt.fail (InvalidMessage (msg ^ ": " ^ s))

let parse ic = expect ic (fun _ -> true)

let print oc m =
  let s = Yojson.Safe.to_string (to_yojson m) in
  Lwt_io.write_line oc s
