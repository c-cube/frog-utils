
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 IRC bot plugin} *)

open Frog
open Frog_server

module E = Misc.Err
module EL = Misc.LwtErr
module C = Calculon

(* state *)
type state = {
  pubsub: Pub_sub.t;
}

(* connect to ZMQ channel *)
let connect () : state E.t Lwt.t =
  let open EL.Infix in
  Pub_sub.create () >|= fun pubsub -> {pubsub}

let cmd_status (st:state): C.Command.t =
  C.Command.make_simple
    ~prefix:"status" ~descr:"status of query" ~prio:10
    (fun _ _msg ->
       Lwt.return_none) (* TODO *)

let maybe_str = function
  | None -> "<none>"
  | Some s -> s

let cmd_froglock : C.Command.t =
  let module M = Lock_messages in
  C.Command.make_simple_l
    ~prefix:"lock_status" ~descr:"status of froglock" ~prio:10
    (fun _ _ ->
       let%lwt res = Lock_client.get_status Lock_daemon.default_port in
       begin match res with
         | Some {M.max_cores=_; current; waiting} ->
           let msg_waiting =
             if waiting=[] then []
             else [Printf.sprintf "[%d waiting jobs]" (List.length waiting)]
           and msg_current =
             let now = Unix.gettimeofday() in
             List.map
               (fun c ->
                  let time = now -. c.M.current_start in
                  let job = c.M.current_job in
                  Printf.sprintf
                    "current job (cores %d, pid %d, \
                     issued %.2fs ago, running for %.2fs): %s"
                    job.M.cores
                    job.M.pid
                    (now -. job.M.query_time)
                    time (maybe_str job.M.info))
               current
           in
           Lwt.return (msg_current @ msg_waiting)
         | None -> Lwt.return []
       end)

let plugin : C.Plugin.t =
  C.Plugin.stateful ~name:"frogirc"
    ~to_json:(fun _ -> None)
    ~of_json:(fun actions _ -> connect ())
    ~commands:(fun st -> [ cmd_status st; cmd_froglock ])
    ()
