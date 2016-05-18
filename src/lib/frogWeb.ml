
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 HTML Interface} *)

module Html = Cow.Html

type html = Html.t

type uri = Uri.t

module HMap = Opium_hmap

(** {2 Encoding Records in HTML} *)

(* make a <pre> block *)
let pre h = Cow.Xml.tag "pre" h

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
      ~row:(fun (s,f) -> [Html.string s; f])
      (List.rev l)
end

(** {2 Serve}

    A small webserver that displays results as they are built, computes
    regressions between results, etc. *)
module Server : sig
  type t
  val create: unit -> t
  val update : t -> (HMap.t -> HMap.t) -> unit
  val map : t -> HMap.t
  val get : t -> 'a HMap.key -> 'a
  val set : t -> 'a HMap.key -> 'a -> unit
  val add_route : t -> string -> Opium.Rock.Handler.t -> unit
  val add_toplevel : t -> string -> descr:string -> unit
  val return_html : ?title:string -> ?code:Cohttp.Code.status_code -> html -> Opium.Response.t Lwt.t
  val set_port : t -> int -> unit
  val run : t -> unit Lwt.t
end = struct
  module H = Html
  open Opium.Std

  type t = {
    mutable map : HMap.t;
    mutable toplevel : (string * string) list; (* toplevel URLs *)
    mutable port: int;
    mutable app : App.t;
  }

  let create() =
    { map=HMap.empty;
      toplevel=[];
      port=8000;
      app=App.empty;
    }

  let update st f = st.map <- f st.map

  let map st = st.map

  let get st k = HMap.get k st.map

  let set st k v = st.map <- HMap.add k v st.map

  let add_route t r h =
    t.app <- Opium.Std.get r h t.app

  let add_toplevel t r ~descr =
    t.toplevel <- (r, descr) :: t.toplevel

  let set_port t p = t.port <- p

  (* TODO: css *)
  let return_html ?title ?code h =
    let wrap_ ?(title="frog-utils") h =
      let hd = H.list [H.title (H.string title)] in
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
        ~f:(fun (path,descr) -> H.a ~href:(Uri.make ~path ()) (H.string descr))
        t.toplevel
      |> H.list
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
