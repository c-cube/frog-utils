
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 HTML Interface} *)

open Result

module E = Misc.Err
module Html = Cow.Html

type 'a or_error = 'a Misc.Err.t

type html = Html.t
type uri = Uri.t
type json = Yojson.Safe.json

(** {2 Helpers} *)

val pre : html -> html
val style : html -> html
val script : string -> html

(** {2 Encoding Records in HTML} *)

module Record : sig
  type t
  val start : t
  val add               : string -> html -> t -> t
  val add_int           : ?raw:bool -> string -> int -> t -> t
  val add_float         : ?raw:bool -> string -> float -> t -> t
  val add_string        : ?raw:bool -> string -> string -> t -> t
  val add_string_option : ?raw:bool -> string -> string option -> t -> t
  val add_bool          : ?raw:bool -> string -> bool -> t -> t
  val close : t -> html
end

(* TODO: same as record, but for full tables? *)

(** {2 Server}

    A small webserver that displays results as they are built, computes
    regressions between results, etc. *)
module Server : sig
  type t
  val create: ?port:int -> storage:Storage.t -> unit -> t
  val storage : t -> Storage.t
  val add_route : t -> ?descr:string -> string -> Opium.Rock.Handler.t -> unit
  val return_html :
    ?title:string -> ?code:Cohttp.Code.status_code -> html -> Opium.Response.t Lwt.t
  val return_json : ?code:Cohttp.Code.status_code -> json -> Opium.Response.t Lwt.t
  val return_string : ?code:Cohttp.Code.status_code -> string -> Opium.Response.t Lwt.t
  val return_404 : string -> Opium.Response.t Lwt.t
  val set_port : t -> int -> unit
  val run : t -> unit Lwt.t
end
