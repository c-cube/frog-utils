
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

type yield_res = result -> unit Lwt.t

(* create a new file in the given directory with the given "name pattern" *)
let make_fresh_file ?dir pattern =
  let dir = match dir with
    | Some d -> d
    | None -> Sys.getcwd ()
  in
  let cmd = Printf.sprintf "mktemp --tmpdir='%s' '%s'" dir pattern |> Lwt_process.shell in
  Lwt_process.with_process_in cmd
    (fun p -> Lwt_io.read_line p#stdout)

let print_job oc job =
  let s = Yojson.Safe.to_string (job_to_yojson job) in
  let%lwt () = Lwt_io.write_line oc s in
  Lwt_io.flush oc

(* given a handle to the result file, adds the result to it *)
let add_res oc res =
  let s = Yojson.Safe.to_string (result_to_yojson res) in
  let%lwt () = Lwt_io.write_line oc s in
  Lwt_io.flush oc

(* TODO: locking *)

let make_job ~file job f =
  let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_SYNC] in
  Lwt_io.with_file ~flags~perm:0o644 ~mode:Lwt_io.output file
    (fun oc ->
      (* print header *)
      let%lwt () = print_job oc job in
      (* call f with a yield_res function*)
      f (add_res oc)
    )

let append_job ~file f =
  let flags = [Unix.O_APPEND; Unix.O_WRONLY; Unix.O_SYNC] in
  Lwt_io.with_file ~flags~perm:0o644 ~mode:Lwt_io.output file
    (fun oc ->
      f (add_res oc)
    )

(* stream of json values from [filename] *)
let read_json_stream filename =
  let stream, push = Lwt_stream.create () in
  (* read json values from another thread *)
  let read_thread =
    Lwt_preemptive.detach
    (fun s ->
      try
        let str = Yojson.Safe.stream_from_file s in
        Stream.iter (fun json -> push (Some json)) str;
        push None
      with e ->
        push None;
        Lwt_log.ign_error_f
          "reading json file %s: %s" filename (Printexc.to_string e);
        ()
    )
    filename
  in
  (* if stream isn't used anymore, stop reading *)
  Gc.finalise (fun _ -> Lwt.cancel read_thread) stream;
  stream

let fold_state_s f init filename =
  try%lwt
    let stream = read_json_stream filename in
    (* parse job header *)
    let%lwt res = Lwt_stream.next stream in
    match job_of_yojson res with
    | `Error e -> Lwt.fail (Failure e)
    | `Ok job ->
        let%lwt acc = init job in
        Lwt_stream.fold_s
          (fun json acc ->
            match result_of_yojson json with
            | `Error e -> Lwt.fail (Failure e)
            | `Ok res -> f acc res
          ) stream acc
  with
    | Failure _ as e -> Lwt.fail e
    | e -> Lwt.fail (Failure (Printexc.to_string e))

let fold_state f init filename =
  fold_state_s
    (fun acc x -> Lwt.return (f acc x))
    (fun job -> Lwt.return (init job))
    filename

module StrMap = Map.Make(String)

let read_state filename =
  fold_state
    (fun (job,map) res -> job, StrMap.add res.res_arg res map)
    (fun job -> job, StrMap.empty)
    filename

let write_state filename (job, res_map) =
  Lwt_io.with_file ~flags:[Unix.O_CREAT; Unix.O_WRONLY; Unix.O_TRUNC]
    ~perm:0o644 ~mode:Lwt_io.output
    filename
    (fun oc ->
      let%lwt () = print_job oc job in
      let%lwt () = Lwt_list.iter_s
        (fun (_, res) -> add_res oc res)
        (StrMap.bindings res_map) in
      Lwt_io.flush oc
    )


