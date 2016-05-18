
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 HTML Interface} *)

module Html = Cow.Html

type 'a or_error = [`Ok of 'a | `Error of string]

type html = Html.t

type uri = Uri.t

type json = Yojson.Safe.json

module HMap = Opium_hmap

(** {2 Encoding Records in HTML} *)

(* custom blocks *)

let pre h = Cow.Xml.tag "pre" h
let style h = Cow.Xml.tag "style" h

module Record : sig
  type t
  val start : t
  val add : string -> html -> t -> t
  val add_int : string -> int -> t -> t
  val add_string : string -> string -> t -> t
  val add_string_option : string -> string option -> t -> t
  val add_bool : string -> bool -> t -> t
  val close : t -> html
end = struct
  type t = (string * html) list
  let start = []
  let add s f l = (s, f) :: l
  let add_with_ fun_ s f l = add s (fun_ f) l
  let add_int = add_with_ Html.int
  let add_string = add_with_ Html.string
  let add_string_option = add_with_ (function None -> Html.empty | Some s -> Html.string s)
  let add_bool = add_with_ (fun b -> Html.string (string_of_bool b))
  let close l =
    Html.Create.table ~flags:[Html.Create.Tags.Headings_fst_col]
      ~row:(fun (s,f) -> [Html.string s; Html.div f])
      (List.rev l)
end

(** {2 Db} *)

module DB : sig
  type t = Dbm.t
  val add_json : f:('a -> json) -> t -> string -> 'a -> unit
  val get_json : f:(json -> 'a or_error) -> t -> string -> 'a or_error
end = struct
  type t = Dbm.t

  let add_json ~f db k v =
    let v' = Yojson.Safe.to_string (f v) in
    Dbm.replace db k v'

  let get_json ~f db k =
    try
      let s = Dbm.find db k in
      let j = Yojson.Safe.from_string s in
      f j
    with
      | Yojson.Json_error s -> `Error ("json error: " ^ s)
      | Not_found ->
        `Error (Printf.sprintf "key `%s` not in database" k)
end

(** {2 Serve}

    A small webserver that displays results as they are built, computes
    regressions between results, etc. *)
module Server : sig
  type t
  val create: db_path:string -> unit -> t
  val update : t -> (HMap.t -> HMap.t) -> unit
  val map : t -> HMap.t
  val get : t -> 'a HMap.key -> 'a
  val set : t -> 'a HMap.key -> 'a -> unit
  val db : t -> DB.t
  val add_route : t -> ?descr:string -> string -> Opium.Rock.Handler.t -> unit
  val return_html : ?title:string -> ?code:Cohttp.Code.status_code -> html -> Opium.Response.t Lwt.t
  val set_port : t -> int -> unit
  val run : t -> unit Lwt.t
end = struct
  module H = Html
  open Opium.Std

  type t = {
    db: DB.t;
    mutable map : HMap.t;
    mutable toplevel : (string * string) list; (* toplevel URLs *)
    mutable port: int;
    mutable app : App.t;
  }

  let create ~db_path () =
    let db = Dbm.opendbm db_path [Dbm.Dbm_create; Dbm.Dbm_rdwr] 0o644 in
    { map=HMap.empty;
      db;
      toplevel=[];
      port=8000;
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

  let css_ =
    "body {
  background-color: #fafafa;
}

h1 { font-size: larger; }
h2 { font-size: large; }

table {
  border-collapse: collapse;
}

tr:nth-child(even) {
  background-color:#f2f2f2;
}

table, th {
  border: 1px solid black;
}

th {
  text-align: left;
  vertical-align: top;
}
"

  (* TODO: css *)
  let return_html ?title ?code h =
    let wrap_ ?(title="frog-utils") h =
      let hd = H.list [H.title (H.string title); style (H.string css_) ] in
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
