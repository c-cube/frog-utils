
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Event Stored on Disk or Transmitted on Network} *)

module E = Misc.Err

type 'a printer = Format.formatter -> 'a -> unit
type 'a or_error = 'a Misc.Err.t

type raw_result = {
  (* Raw output *)
  errcode: int;
  stdout: string;
  stderr: string;

  (* Time used *)
  rtime : float;
  utime : float;
  stime : float;
} [@@deriving yojson,eq]

type prover  = Prover.t [@@deriving yojson,eq]
type checker = unit [@@deriving yojson,eq]

type +'a result = {
  program : 'a;
  problem : Problem.t;
  raw : raw_result;
} [@@deriving yojson,eq]

let analyze_p t =
  let prover = t.program in
  (* find if [re: re option] is present in [stdout] *)
  let find_opt_ re = match re with
    | None -> false
    | Some re ->
      Re.execp (Re_posix.compile_pat re) t.raw.stdout ||
      Re.execp (Re_posix.compile_pat re) t.raw.stderr
  in
  if t.raw.errcode = 0 then
    if find_opt_ prover.Prover.sat then Res.Sat
    else if find_opt_ prover.Prover.unsat then Res.Unsat
    else if find_opt_ prover.Prover.timeout then Res.Timeout
    else Res.Unknown
  else if find_opt_ prover.Prover.timeout then Res.Timeout
  else if find_opt_ prover.Prover.unknown then Res.Unknown
  else Res.Error

type t =
  | Prover_run of prover result
  | Checker_run of checker result
[@@deriving yojson,eq]

type event = t [@@deriving yojson,eq]

let program e = e.program
let problem e = e.problem
let raw e = e.raw

let to_string r = to_yojson r |> Yojson.Safe.to_string
let pp out r = Format.pp_print_string out (to_string r)

let mk_prover r = Prover_run r
let mk_checker r = Checker_run r

type uuid = Uuidm.t
let equal_uuid = Uuidm.equal

let uuid_to_yojson u = `String (Uuidm.to_string u)
let uuid_of_yojson j = match j with
  | `String s ->
    begin match Uuidm.of_string s with
      | Some x-> E.return x
      | None -> E.fail "invalid uuid"
    end
  | _ -> E.fail "expected string for uuid"

type timestamp = float [@@deriving eq]
let timestamp_to_yojson f = `String (string_of_float f)
let timestamp_of_yojson = function
  | `String s -> (try E.return (float_of_string s) with _ -> E.fail "expected float")
  | _ -> E.fail "expected float"

type snapshot = {
  uuid: uuid;
  timestamp: timestamp;
  events: t list;
  meta: (string [@default ""]); (* additional metadata *)
} [@@deriving yojson,eq]

let assoc_or def x l =
  try List.assoc x l
  with Not_found -> def

module Snapshot = struct
  type t = snapshot [@@deriving yojson,eq]

  let to_file ~file t =
    Yojson.Safe.to_file file (to_yojson t) |> Misc.LwtErr.return

  let of_file ~file : t Misc.LwtErr.t =
    let open E in
    try
      let j = Yojson.Safe.from_file file in
      of_yojson j |> Lwt.return
    with e ->
      E.fail (Printexc.to_string e) |> Lwt.return

  let make ?(uuid=Uuidm.create `V4) ?(meta="") ?(timestamp=Unix.gettimeofday()) l =
    { timestamp; uuid; events=l; meta; }

  let pp out (r:t) =
    Format.fprintf out
      "{@[<hv>timestamp:%.2f;@ uuid: %s;@ events: @[<v>%a@]@]}"
      r.timestamp (Uuidm.to_string r.uuid)
      (Misc.Fmt.pp_list pp) r.events

  let provers t =
    List.fold_left
      (fun set e -> match e with
         | Prover_run {program=p;_} ->
           Prover.Set.add p set
         | Checker_run _ -> set)
      Prover.Set.empty
      t.events
end

type prover_set = Prover.Set.t
let prover_set_to_yojson s = `List (List.map Prover.to_yojson (Prover.Set.elements s))
let prover_set_of_yojson = function
  | `List l ->
    let open Misc.Err in
    let l = List.map Prover.of_yojson l in
    seq_list l >|= Prover.Set.of_list
  | _ -> Misc.Err.fail "invalid set of provers"

type snapshot_meta = {
  s_uuid: uuid [@key "uuid"];
  s_timestamp: float [@key "timestamp"];
  s_meta: (string [@default ""])[@key "meta"];
  s_provers: prover_set [@key "provers"][@equal Prover.Set.equal];
  s_len: int [@key "length"];
} [@@deriving yojson,eq]

module Meta = struct
  type t = snapshot_meta [@@deriving yojson,eq]
  let uuid s = s.s_uuid
  let timestamp s = s.s_timestamp
  let provers s = s.s_provers
  let length s = s.s_len

  let pp out r: unit =
    Format.fprintf out
      "{@[<hv>timestamp:%.2f;@ uuid: %s;@ len %d@]}"
      r.s_timestamp (Uuidm.to_string r.s_uuid) r.s_len
end

let meta s = {
  s_uuid=s.uuid;
  s_timestamp=s.timestamp;
  s_meta=s.meta;
  s_provers=Snapshot.provers s;
  s_len=List.length s.events;
}
