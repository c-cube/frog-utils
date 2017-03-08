
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 HTML Interface} *)

open Result
open Frog
open Frog_server

module E = Misc.Err

type 'a or_error = 'a Misc.Err.t

type html = Html.t
type uri = Uri.t
type json = Yojson.Safe.json

(* TODO: same as record, but for full tables? *)

(** {2 Server}

    A small webserver that displays results as they are built, computes
    regressions between results, etc. *)
module Server : sig
  type t
  val create: ?port:int -> storage:Storage.t -> unit -> t
  val storage : t -> Storage.t
  val return_html :
    ?title:string -> ?code:Cohttp.Code.status_code -> html -> Opium.Response.t Lwt.t
  val return_json : ?code:Cohttp.Code.status_code -> json -> Opium.Response.t Lwt.t
  val return_string : ?code:Cohttp.Code.status_code -> string -> Opium.Response.t Lwt.t
  val return_404 : string -> Opium.Response.t Lwt.t
  val set_port : t -> int -> unit
  val run : t -> unit Lwt.t
end
