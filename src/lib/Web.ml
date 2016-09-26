
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 HTML Interface} *)

open Result

module E = Misc.Err
module Html = Cow.Html

type 'a or_error = 'a Misc.Err.t

type html = Html.t

type uri = Uri.t

type json = Yojson.Safe.json

(** {2 Encoding Records in HTML} *)

(* custom blocks *)

let pre h = Cow.Xml.tag ~attrs:["class", "raw"] "pre" h
let style h = Cow.Xml.tag "style" h
let script s =
  Cow.Xml.tag "script"
    ~attrs:["type", "text/javascript"; "src", s]
    Cow.Xml.empty

module Record = struct
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

(* TODO: same as record, but for full tables? *)

(** {2 Serve}

    A small webserver that displays results as they are built, computes
    regressions between results, etc. *)
module Server = struct
  module H = Html
  open Opium.Std

  type t = {
    storage: Storage.t;
    mutable toplevel : (string * string) list; (* toplevel URLs *)
    mutable port: int;
    mutable app : App.t;
  }

  let create ?(port=8000) ~storage () =
    {
      storage;
      toplevel=[];
      port;
      app=App.empty;
    }

  let storage t = t.storage

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
      margin: 0px;
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
          [ meta_;
            script "/static/frogwebclient.js";
            H.title (H.string title);
            style (H.string css_);
          ]
      in
      H.list
        [ H.head hd
        ; H.body h
        ] |> H.to_string
    in
    let h = wrap_ ?title h in
    App.respond' ?code (`Html h)

  let return_string ?code (s:string) = App.respond' ?code (`String s)

  let return_json ?code (j:json) = return_string ?code (Yojson.Safe.to_string j)

  let return_404 msg =
    let code = Cohttp.Code.status_of_code 404 in
    let h = Html.string msg in
    return_html ~code h

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
    |> App.middleware
      (Middleware.static ~local_path:"static/" ~uri_prefix:"/static/")
    |> App.get "/" (main t) (* main page *)
    |> App.port t.port
    |> App.start
end
