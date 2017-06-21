
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Snapshots, i.e lists of events} *)

module E = Misc.Err

type 'a printer = Format.formatter -> 'a -> unit
type 'a or_error = 'a Misc.Err.t

type uuid = Uuidm.t

let uuid_to_yojson u = `String (Uuidm.to_string u)
let uuid_of_yojson j = match j with
  | `String s ->
    begin match Uuidm.of_string s with
      | Some x-> E.return x
      | None -> E.fail "invalid uuid"
    end
  | _ -> E.fail "expected string for uuid"

type timestamp = float
let timestamp_to_yojson f = `String (string_of_float f)
let timestamp_of_yojson = function
  | `String s -> (try E.return (float_of_string s) with _ -> E.fail "expected float")
  | _ -> E.fail "expected float"

type t = {
  uuid: uuid;
  timestamp: timestamp;
  meta: (string [@default ""]); (* additional metadata *)
} [@@deriving yojson]

let make
    ?(uuid=Uuidm.create `V4) ?(meta="")
    ?(timestamp=Unix.gettimeofday()) () =
  { timestamp; uuid; meta; }

let pp out (r:t) =
  Format.fprintf out
    "{@[<hv>timestamp:%.2f;@ uuid: %s@]}"
    r.timestamp (Uuidm.to_string r.uuid)


type prover_set = Prover.Set.t
let prover_set_to_yojson s = `List (List.map Prover.to_yojson (Prover.Set.elements s))
let prover_set_of_yojson = function
  | `List l ->
    let open Misc.Err in
    let l = List.map Prover.of_yojson l in
    seq_list l >|= Prover.Set.of_list
  | _ -> Misc.Err.fail "invalid set of provers"


module Meta = struct

  type t = {
    s_uuid: uuid [@key "uuid"];
    s_timestamp: float [@key "timestamp"];
    s_meta: (string [@default ""])[@key "meta"];
    s_provers: prover_set [@key "provers"];
    s_len: int [@key "length"];
  } [@@deriving yojson]

  let uuid s = s.s_uuid
  let timestamp s = s.s_timestamp
  let provers s = s.s_provers
  let length s = s.s_len

  let pp out r: unit =
    Format.fprintf out
      "{@[<hv>timestamp:%.2f;@ uuid: %s;@ len %d@]}"
      r.s_timestamp (Uuidm.to_string r.s_uuid) r.s_len
end

