
(* This file is free software, part of frog-utils. See file "license" for more details. *)

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

let invalid_msg_ s = raise (InvalidMessage s)
let unexpected_ x = raise (Unexpected x)

let register_exn_printers () =
  Printexc.register_printer
    (function
      | InvalidMessage s -> Some ("invalid message: " ^ s)
      | Unexpected s -> Some ("unexpected message: " ^ show s)
      | _ -> None);
  ()

let expect ic p =
  let s = Pervasives.input_line ic in
  let res = Yojson.Safe.from_string s in
  match of_yojson res with
  | `Ok m when p m -> m
  | `Ok m -> unexpected_ m
  | `Error msg -> invalid_msg_ msg

let parse ic = expect ic (fun _ -> true)

let print oc m =
  Yojson.Safe.to_channel oc (to_yojson m);
  output_char oc '\n';
  flush oc
