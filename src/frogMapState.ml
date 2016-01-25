
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Persistent State for FrogMap} *)

[@@@warning "-39"]

type job = {
  cmd        : string [@key "cmd"];
  arguments  : string list [@key "arguments"];
  cwd        : string [@key "cwd"];
} [@@deriving yojson,show]
(** Description of a job *)

type result = {
  res_arg     : string [@key "arg"];
  res_rtime   : float [@key "time"];
  res_utime   : (float [@default 0.]) [@key "utime"];
  res_stime   : (float [@default 0.]) [@key "stime"];
  res_errcode : int [@key "errcode"];
  res_out     : string [@key "stdout"];
  res_err     : string [@key "stderr"];
} [@@deriving yojson {strict=false},show]
(** Result of running the command on one argument *)

[@@@warning "+39"]

type yield_res = result -> unit

let with_proc_in cmd f =
  let ic = Unix.open_process_in cmd in
  CCFun.finally
    ~f:(fun () -> f ic)
    ~h:(fun () -> ignore (Unix.close_process_in ic))

(* create a new file in the given directory with the given "name pattern" *)
let make_fresh_file ?dir pattern =
  let dir = match dir with
    | Some d -> d
    | None -> Sys.getcwd ()
  in
  let cmd = Printf.sprintf "mktemp --tmpdir='%s' '%s'" dir pattern in
  with_proc_in cmd input_line

let print_job oc job =
  Yojson.Safe.to_channel oc (job_to_yojson job);
  flush oc

(* given a handle to the result file, adds the result to it *)
let add_res oc res =
  Yojson.Safe.to_channel oc (result_to_yojson res);
  flush oc

(* TODO: locking *)

let make_job ~file job f =
  let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_SYNC] in
  CCUnix.with_out ~flags ~mode:0o644 file
    ~f:(fun oc ->
      (* print header *)
      print_job oc job;
      (* call f with a yield_res function*)
      f (add_res oc))

let append_job ~file f =
  let flags = [Unix.O_APPEND; Unix.O_WRONLY; Unix.O_SYNC] in
  CCUnix.with_out ~flags ~mode:0o644 file
    ~f:(fun oc -> f (add_res oc))

type +'a gen = unit -> 'a option

(* generator of json values from [filename] *)
let read_json_gen file : _ gen =
  (* read json values from another thread *)
  let str = Yojson.Safe.stream_from_file file in
  let gen () =
    try Some (Stream.next str)
    with
    | Stream.Error msg as e ->
        Logs.err
          (fun k->k  "reading json file %s: %s" file msg);
        raise e
    | Stream.Failure -> None
  in
  gen

let gen_head g = match g() with
  | Some x -> x
  | None -> failwith "empty stream"

let rec gen_fold f acc g = match g() with
  | None -> acc
  | Some x ->
      let acc = f acc x in
      gen_fold f acc g

let fold_state f init filename =
  let g = read_json_gen filename in
  (* first item should be the job description *)
  match job_of_yojson (gen_head g) with
  | `Error e -> raise (Failure e)
  | `Ok job ->
      let acc = init job in
      gen_fold
        (fun acc json -> match result_of_yojson json with
          | `Error e -> failwith e
          | `Ok x -> f acc x)
        acc g

module StrMap = Map.Make(String)

let read_state filename =
  fold_state
    (fun (job,map) res -> job, StrMap.add res.res_arg res map)
    (fun job -> job, StrMap.empty)
    filename

let write_state filename (job, res_map) =
  CCUnix.with_out
    ~flags:[Unix.O_CREAT; Unix.O_WRONLY; Unix.O_TRUNC]
    ~mode:0o644
    filename
    ~f:(fun oc ->
      print_job oc job;
      StrMap.iter
        (fun _ res -> add_res oc res)
        res_map;
      flush oc)


