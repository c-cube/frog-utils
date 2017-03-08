
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Pub/Sub bus} *)

open Frog
module E = Misc.Err

type msg =
  | M_start_bench of int [@name "start_bench"] (* number of files *)
  | M_finish_bench [@name "finish_bench"] (* *)
  | M_event of Event.t [@name "event"]
  [@@deriving yojson]

type t = {
  ctx: ZMQ.Context.t;
  pub: [`Pub] Lwt_zmq.Socket.t;
  sub: [`Sub] Lwt_zmq.Socket.t;
}

let default_ = "ipc:///tmp/frogbus"

let create ?(path=default_) () : t E.t Lwt.t =
  try
    let ctx = ZMQ.Context.create () in
    let sock_pub = ZMQ.Socket.create ctx ZMQ.Socket.pub in
    let sock_sub = ZMQ.Socket.create ctx ZMQ.Socket.sub in
    ZMQ.Socket.bind sock_pub path;
    ZMQ.Socket.connect sock_sub path;
    ZMQ.Socket.subscribe sock_sub "";
    let t = {
      ctx;
      pub=Lwt_zmq.Socket.of_socket sock_pub;
      sub=Lwt_zmq.Socket.of_socket sock_sub;
    } in
    E.return t |> Lwt.return
  with e ->
    E.of_exn (E.fail e) |> Lwt.return

let close (t:t): unit Lwt.t =
  ZMQ.Socket.close @@ Lwt_zmq.Socket.to_socket t.pub;
  ZMQ.Socket.close @@ Lwt_zmq.Socket.to_socket t.sub;
  ZMQ.Context.terminate t.ctx;
  Lwt.return_unit

let with_ ?path f =
  let open Misc.LwtErr.Infix in
  create ?path () >>= fun t ->
  try%lwt
    let%lwt res = f t in
    let%lwt () = close t in
    Lwt.return res
  with e ->
    let%lwt () = close t in
    Lwt.return (E.fail e |> E.of_exn)

let send (t:t) (m:msg): unit Lwt.t =
  let str = msg_to_yojson m |> Yojson.Safe.to_string in
  Lwt_log.ign_debug_f "@[<2>pub_sub.send@ %s@]" str;
  Lwt_zmq.Socket.send t.pub str

let next (t:t): msg E.t Lwt.t =
  let%lwt str = Lwt_zmq.Socket.recv t.sub in
  Lwt_log.ign_debug_f "@[<2>pub_sub.receive@ %s@]" str;
  try
    let j = Yojson.Safe.from_string str in
    let msg = msg_of_yojson j in
    Lwt.return msg
  with e ->
    Misc.LwtErr.lift (E.of_exn (E.fail e))



