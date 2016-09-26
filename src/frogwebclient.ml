
(** {1 Client-Side Frogweb} *)

open Lwt.Infix

module H = Web.Html

let document = Dom_html.document

let refresh_button = Dom_html.createButton Dom_html.document

let refresh() =
  let main = Dom_html.getElementById "main" in
  Lwt.return () (* TODO *)

let () =
  Lwt.async
    (fun _ ->
       Lwt_js_events.domContentLoaded () >>= fun _ ->
        (*
    main##appendChild (To_dom.of_node search_box) |> ignore;
    main##appendChild (To_dom.of_node results_html) |> ignore;
           *)
       Lwt.return ()
  );

  Lwt.async
    (fun _ ->
       Lwt_js_events.limited_loop ~elapsed_time:0.2
         Lwt_js_events.input refresh_button
         (fun _ _ -> refresh ())
    )

