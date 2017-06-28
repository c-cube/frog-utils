
exception Wrong_message

module type T = sig
  type t
end

module type Queue = sig
  type 'a t
  val is_empty : _ t -> bool
  val pop : 'a t -> 'a option
  val push : 'a -> 'a t -> unit
end

module Queue = struct

  type 'a t = 'a list ref

  let is_empty t = (!t = [])

  let push x r = (r := x :: !r)

  let pop t =
    match !t with
    | [] -> None
    | x :: r ->
      t := r; Some x

end

module Make(Q: Queue)(Task : T)(Return : T) = struct

  (* Tasks *)

  type task = {
    id : int;
    load : Task.t;
    start : float;
  }

  let mk_task =
    let r = ref 0 in
    (fun load ->
       let id = incr r; !r in
       let start = Unix.gettimeofday () in
       { id; start; load; })


  (** Messages *)

  type descr =
    | Waiting
    | Do        of task
    | Done      of (Return.t, exn) result
    | No_more_tasks

  type msg = {
    tid : int;
    descr : descr;
  }

  type channel = msg Event.channel

  let mk_msg descr =
    let tid = Thread.id @@ Thread.self () in
    { tid; descr }

  let pp_msg fmt msg =
    Format.fprintf fmt "{%d}: %s" msg.tid (
      match msg.descr with
      | Waiting       -> "waiting"
      | Do _          -> "do"
      | Done _        -> "done"
      | No_more_tasks -> "no more tasks"
    )

  let listen channel =
    Event.sync @@ Event.receive channel

  let send channel descr =
    Event.sync @@ Event.send channel @@ mk_msg descr


  (** Pool worker *)

  let run_worker f =
    (* Start state, send ready to the server *)
    let rec ready channel =
      let () = send channel Waiting in
      wait channel
    (* Waiting state, wait for a task *)
    and wait channel =
      match listen channel with
      | { descr = Do t } -> work channel t
      | { descr = No_more_tasks } -> ()
      | msg -> error channel msg
    (* A task has been received, execute the worker function,
       and go back to the ready state for a new task. *)
    and work channel task =
      let msg = try Ok (f task.load) with exn -> Error exn in
      let () = send channel (Done msg) in
      ready channel
    (* Error state, we received the wrong message, send
       an error and then go back to being ready. *)
    and error channel msg =
      Format.eprintf "Wrong answer from server: %a" pp_msg msg;
      ready channel
    in
    ready


  (** Pool server *)

  type pool = {
    tbl: (int, int) Hashtbl.t;
    tasks   : task option array;
    threads : Thread.t array;
    channels : channel array;
  }

  let get_idx pool tid =
    try Hashtbl.find pool.tbl tid
    with Not_found -> assert false

  let get_channel pool tid =
    let idx = get_idx pool tid in
    pool.channels.(idx)

  let pop_task pool tid =
    let idx = get_idx pool tid in
    match pool.tasks.(idx) with
    | None -> assert false
    | Some x ->
      let () = pool.tasks.(idx) <- None in
      x

  let add_task pool tid t =
    let idx = get_idx pool tid in
    match pool.tasks.(idx) with
    | Some _ -> assert false
    | None -> pool.tasks.(idx) <- Some t

  let running_tasks pool =
    Array.fold_left (
      fun i -> function
        | None -> i
        | Some _ -> i + 1
    ) 0 pool.tasks

  let server_run handler pool queue event =
    let rec wait () =
      let msg = Event.sync event in
      match msg.descr with
      | Waiting -> serve msg.tid
      | Done r -> handle msg.tid r
      | _ -> assert false
    and serve tid =
      let ch = get_channel pool tid in
      match Q.pop queue with
      | Some x ->
        let t = mk_task x in
        let () = add_task pool tid t in
        let () = send ch (Do t) in
        wait ()
      | None ->
        let () = send ch No_more_tasks in
        check ()
    and handle tid r =
      let task = pop_task pool tid in
      match handler task.load r with
      | `Ok -> check ()
      | `Retry ->
        Q.push task.load queue;
        wait ()
    and check () =
      if Q.is_empty queue && running_tasks pool = 0 then ()
      else wait ()
    in
    wait


  (** Create workers *)
  let mk_pool j f =
    assert (j > 0);
    let rec aux acc n =
      if n > 0 then
        let ch = Event.new_channel () in
        let t = Thread.create (run_worker f) ch in
        aux ((t, ch) :: acc) (n - 1)
      else (* n <= 0 *)
        let l, l' = List.split acc in
        let tasks = Array.make j None in
        let threads = Array.of_list l in
        let channels = Array.of_list l' in
        let tbl = Hashtbl.create j in
        let () = Array.iteri (fun i t -> Hashtbl.add tbl (Thread.id t) i) threads in
        { tbl; tasks; threads; channels; }
    in
    aux [] j

  let mk_event pool =
    let l = Array.to_list pool.channels in
    let l' = List.map Event.receive l in
    Event.choose l'

  (** Main entrypoint*)
  let run ~j ~server ~worker tasks =
    let pool = mk_pool j worker in
    let event = mk_event pool in
    server_run server pool tasks event

end


