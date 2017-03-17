(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Communication Protocole} *)
open Frog

[@@@warning "-39"]

type uid = int [@@deriving yojson,show,eq]

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
  uid         : uid [@key "uid"]; (* unique ID of query *)
  cores       : int [@default 0] [@key "cores"];
}
[@@deriving yojson {strict=false},show,eq]

type current_job = {
  current_job   : job [@key "job"];
  current_start : float [@key "start"];  (* time at which task started *)
} [@@deriving yojson,show,eq]

type waiting_job = {
  waiting_id    : int [@key "id"];
  waiting_job   : job [@key "job"];
} [@@deriving yojson, show,eq]

type status_answer = {
  max_cores : int [@key "cores"];
  current : current_job list [@key "current"];
  waiting : waiting_job list [@key "waiting"];
} [@@deriving yojson,show,eq]

type t =
  | Start [@name "start"]
  | End [@name "end"]
  | Acquire of job [@name "acquire"]
  | Go of uid [@name "go"]
  | Release of uid [@name "release"]
  | Reject of uid [@name "reject"]
  | Status [@name "status"]
  | StatusAnswer  of status_answer [@name "statusanswer"]
  | StopAccepting [@name "stopaccepting"] (* from now on, no more accepts *)
  | Start_bench of int [@name "start_bench"] (* number of files *)
  | Finish_bench [@name "finish_bench"] (* *)
  | Event of Event.t [@name "event"]
  [@@deriving yojson, show,eq]

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
  | Result.Ok m when p m -> Lwt.return m
  | Result.Ok _ -> Lwt.fail (InvalidMessage ("unexpected " ^ s))
  | Result.Error msg -> Lwt.fail (InvalidMessage (msg ^ ": " ^ s))

let parse ic = expect ic (fun _ -> true)

let print oc m =
  let s = Yojson.Safe.to_string (to_yojson m) in
  let%lwt () = Lwt_io.write_line oc s in
  Lwt_io.flush oc

