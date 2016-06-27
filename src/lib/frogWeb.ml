
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 HTML Interface} *)

open Result

module E = FrogMisc.Err
module Html = Cow.Html

type 'a or_error = 'a FrogMisc.Err.t

type html = Html.t

type uri = Uri.t

type json = Yojson.Safe.json

module HMap = Opium_hmap

(** {2 Encoding Records in HTML} *)

(* custom blocks *)

let pre h = Cow.Xml.tag ~attrs:["class", "raw"] "pre" h
let style h = Cow.Xml.tag "style" h

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
end = struct
  type t = (string * html) list
  let start = []
  let add s f l = (s, f) :: l
  let add_with_ fun_ ?(raw=false) s f l =
    let body = fun_ f in
    add s (if raw then pre body else Html.div body) l
  let add_int = add_with_ Html.int
  let add_float = add_with_ Html.float
  let add_string = add_with_ Html.string
  let add_string_option = add_with_ (function None -> Html.empty | Some s -> Html.string s)
  let add_bool = add_with_ (fun b -> Html.string (string_of_bool b))
  let close l =
    Html.Create.table ~flags:[Html.Create.Tags.Headings_fst_col]
      ~row:(fun (s,f) -> [Html.string s; f])
      (List.rev l)
end

(** {2 Serve}

    A small webserver that displays results as they are built, computes
    regressions between results, etc. *)
module Server : sig
  type t
  val create: ?port:int -> db:FrogDB.t -> unit -> t
  val update : t -> (HMap.t -> HMap.t) -> unit
  val map : t -> HMap.t
  val get : t -> 'a HMap.key -> 'a
  val set : t -> 'a HMap.key -> 'a -> unit
  val db : t -> FrogDB.t
  val add_route : t -> ?descr:string -> string -> Opium.Rock.Handler.t -> unit
  val return_html : ?title:string -> ?code:Cohttp.Code.status_code -> html -> Opium.Response.t Lwt.t
  val set_port : t -> int -> unit
  val run : t -> unit Lwt.t
end = struct
  module H = Html
  open Opium.Std

  type t = {
    db: FrogDB.t;
    mutable map : HMap.t;
    mutable toplevel : (string * string) list; (* toplevel URLs *)
    mutable port: int;
    mutable app : App.t;
  }

  let create ?(port=8000) ~db () =
    { map=HMap.empty;
      db;
      toplevel=[];
      port;
      app=App.empty;
    }

  let update st f = st.map <- f st.map

  let db t = t.db

  let map t = t.map

  let get t k = HMap.get k t.map

  let set t k v = t.map <- HMap.add k v t.map

  let add_route t ?descr r h =
    t.app <- Opium.Std.get r h t.app;
    begin match descr with
      | Some descr when not (String.contains r ':') ->
        t.toplevel <- (r, descr) :: t.toplevel
      | _ -> ()
    end;
    ()

  let set_port t p = t.port <- p

  let meta_ = H.meta ~attrs:["charset","UTF-8"] H.empty

  let css_ = "
body {
  width: 80%;
  margin-left : auto;
  margin-right: auto;
  background-color: #fafafa;
}

h1 { font-size: larger; }
h2 { font-size: large; }

div {
  padding: 5px;
}

pre.raw {
  magin: 0px;
  padding: 5px;
  background-color: lightgrey;
}

table {
  border: 1px solid black;
  border-collapse: collapse;
}

tr:hover {
  background-color: #f2f2f2;
}

td: nth-of-type(even) {
  background-color: #f2f2f2;
}

tr {
  border: 1px solid lightgrey;
}

td:nth-of-type(1) {
  border: 1ps solid lightgrey;
}

th, td {
  padding: 10px 20px;
  text-align: left;
  vertical-align: center;
}
"

  (* TODO: css *)
  let return_html ?title ?code h =
    let wrap_ ?(title="frog-utils") h =
      let hd =
        H.list
          [ meta_
          ; H.title (H.string title)
          ; style (H.string css_) ]
      in
      H.list
        [ H.head hd
        ; H.body h
        ] |> H.to_string
    in
    let h = wrap_ ?title h in
    App.respond' ?code (`Html h)

  (* toplevel handler *)
  let main t _req =
    let tops =
      List.map
        ~f:(fun (path,descr) ->
          H.a ~href:(Uri.make ~path ()) (H.string descr) |> H.p)
        t.toplevel
      |> H.ul
    in
    let h =
      H.list
        [ H.h1 (H.string "frog-utils")
        ; tops
        ]
    in
    return_html h

  (* loop that serves the website *)
  let run t =
    Lwt_io.printlf "serve website on http://localhost:%d" t.port >>= fun () ->
    t.app
    |> App.get "/" (main t) (* main page *)
    |> App.port t.port
    |> App.start
end
