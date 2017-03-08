
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Pub/Sub bus} *)

(** A global bus shared among various frog-tools, to notify one another
    of events *)

open Frog
module E = Misc.Err

type t

type msg =
  | M_start_bench of int [@name "start_bench"] (* number of files *)
  | M_finish_bench [@name "finish_bench"] (* *)
  | M_event of Event.t [@name "event"]
  [@@deriving yojson, show]

val create : ?path:string -> unit -> t E.t Lwt.t

val with_ : ?path:string -> (t -> 'a E.t Lwt.t) -> 'a E.t Lwt.t

val send : t -> msg -> unit Lwt.t

val next : t -> msg E.t Lwt.t

