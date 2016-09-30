
(** {1 Client-Side Frogweb} *)

open Lwt.Infix
open Frog

module R = Tyxml_js.R
module H = Tyxml_js.Html
module L = ReactiveData.RList

(* Type definitions *)

type pv_filter = {
  pv_name : string;
}

type pb_filter = {
  pb_name : string;
}

(****************************************************************************)
(* HTML rendering *)

let pv_to_html p =
  let open Prover in
  match p.version with
  | Tag s ->
    [ H.pcdata (Format.sprintf "%s %s" p.name s) ]
  | Git (branch, commit) -> [
      H.pcdata (Format.sprintf "%s@@%s" p.name branch);
      H.br ();
      H.pcdata (Format.sprintf "%s.." (String.sub commit 0 15));
    ]

let res_to_html r =
  let open Res in
  let color = match r with
    | Unsat | Sat -> "darkgreen"
    | Timeout
    | Unknown -> "orange"
    | Error -> "red"
  in
  H.pcdata (to_string r)
  |> Misc.List.return
  |> H.span ~a:[H.a_class ["result"]; H.a_style ("color:"^color)]

(****************************************************************************)
(* HTML inputs *)

let input_string placeholder set =
  let input =
    H.input ~a:[H.a_input_type `Text;
                H.a_placeholder placeholder;]
      () in
  let node = Tyxml_js.To_dom.of_input input in
  Lwt.async (fun _ ->
      Lwt_js_events.limited_loop ~elapsed_time:0.2
        Lwt_js_events.input node (fun _ _ ->
            let s = Js.to_string ((Tyxml_js.To_dom.of_input input)##.value) in
            set s;
            Lwt.return ()));
  input

let input_button name callback =
  let b = H.button [ H.pcdata name ] in
  let node = Tyxml_js.To_dom.of_button b in
  Lwt.async (fun _ ->
      Lwt_js_events.limited_loop ~elapsed_time:0.2
        Lwt_js_events.click node (fun _ _ -> callback ()));
  b

(****************************************************************************)
(* Data & fetching *)

let status, set_status =
  React.S.create "Starting..."

let snapshots, set_snapshots =
  React.S.create []

let snap_list s =
  let json = Yojson.Safe.from_string s in
  match [%of_yojson: string list] json with
  | Result.Ok l ->
    set_status "Parsed snapshot list !";
    l
  | Result.Error msg ->
    set_status (
      Format.sprintf "While parsing snapshots list: %s" msg);
    []

let get_snapshots () =
  set_snapshots [];
  let%lwt frame = XmlHttpRequest.get "/snapshots/" in
  let l = snap_list frame.XmlHttpRequest.content in
  let n = List.length l in
  let%lwt () = Lwt_list.iteri_s (fun i s ->
      set_status (Format.sprintf "Fetching snapshot %d/%d ..." (i + 1) n);
      let%lwt frame = XmlHttpRequest.get (Format.sprintf "/snapshot/%s" s) in
      set_status (Format.sprintf "Parsing snapshot %d/%d ..." (i + 1) n);
      let json = Yojson.Safe.from_string frame.XmlHttpRequest.content in
      match [%of_yojson: Event.Snapshot.t] json with
      | Result.Ok s ->
        set_status (Format.sprintf "Adding snapshot %d/%d ..." (i + 1) n);
        let old = React.S.value snapshots in
        set_snapshots (s :: old);
        Lwt.return_unit
      | Result.Error _ ->
        set_status (Format.sprintf "Error while reading snapshot %d" (i + 1));
        Lwt.return_unit
    ) l in
  Lwt.return_unit

(****************************************************************************)
(* Interface: snapshot list *)

let mode_snap () = [
  H.div [ H.pcdata "not implemented yet..." ];
]


(****************************************************************************)
(* Interface: Full table *)

let split_events =
  let rec aux acc_p acc_c = function
    | [] -> acc_p, acc_c
    | Event.Prover_run res :: r -> aux (res :: acc_p) acc_c r
    | Event.Checker_run res :: r -> aux acc_p (res :: acc_c) r
  in
  aux [] []


let mode_all () =
  (* flattened list of all events *)
  let results =
    let aux v =
      let res, _l = List.fold_left (fun (acc, acc') s ->
          let l, l' = split_events s.Event.events in
          (l @ acc, l' @ acc')) ([], []) v
      in
      res
    in
    React.S.map aux snapshots
  in
  (* Filters for problems and provers *)
  let pv_filter, set_pv_filter =
    let init = { pv_name = ""; } in
    React.S.create init in
  let pb_filter, set_pb_filter =
    let init = { pb_name = ""; } in
    React.S.create init in

  let pv_pre_list =
    let cmp p p' =
      let open Prover in
      compare (p.name, p.version) (p'.name, p'.version)
    in
    let aux t =
      OLinq.(of_list t
             |> map (fun r -> r.Event.program)
             |> distinct ~cmp ()
             |> sort ~cmp ()
             |> run_list)
    in
    React.S.map aux results
  in

  let pv_list =
    let aux f t =
      let pred p =
        let r = Regexp.regexp_case_fold f.pv_name in
        match Regexp.string_match r p.Prover.name 0 with
        | Some _ -> true | None -> false
      in
      OLinq.(of_list t
             |> filter pred
             |> run_list)
    in
    React.S.l2 aux pv_filter pv_pre_list
  in

  let pb_pre_list =
    let cmp = Problem.compare_name in
    let proj p = p.Event.problem in
    let aux t =
      OLinq.(of_list t
             |> group_by ~cmp proj
             |> run_list)
    in
    React.S.map aux results
  in

  let pb_table =
    let cmp p p' =
      let open Prover in
      compare (p.name, p.version) (p'.name, p'.version)
    in
    let merge _ l l' = match l with
      | [] -> None
      | _ -> (Some l')
    in
    let aux pvs f t =
      let pv_l = OLinq.of_list pvs in
      let pred (p, _) =
        let r = Regexp.regexp_case_fold f.pb_name in
        match Regexp.string_match r p.Problem.name 0 with
        | Some _ -> true | None -> false
      in
      OLinq.(of_list t
             |> filter pred
             |> map (fun (pb, l) -> pb,
                                    of_list l
                                    |> outer_join ~cmp
                                      (fun x -> x)
                                      (fun r -> r.Event.program)
                                      ~merge pv_l
                                    |> run_list)
             |> sort_by ~cmp:Problem.compare_name fst
             |> run_list)
    in
    React.S.l3 aux pv_list pb_filter pb_pre_list
  in
  let table =
    (* Compute the headers of the table *)
    let pvs = L.from_signal pv_list in
    let t, _ = L.create [ [ H.pcdata "" ] ] in
    let th, _ = L.create @@ [
        R.Html.tr (
          L.map H.th @@
          L.concat t (L.map pv_to_html pvs)
        )
      ]
    in
    (* Compute the rows *)
    let pbs = L.from_signal pb_table in
    let trs = L.map (fun (pb, l) ->
        H.tr (
          H.td [ H.pcdata @@ Filename.basename pb.Problem.name ] ::
          (List.map (function
               | [] -> H.td [ H.pcdata "." ]
               | l -> H.td (
                   List.map (fun r ->
                       H.span [
                         res_to_html @@ Event.analyze_p r;
                         H.br ();
                       ]) l)
             ) l)
        )) pbs
    in
    let l = L.concat th trs in
    R.Html.table l
  in
  (* Buttons & co *)
  let input_pv = input_string "prover" (fun s ->
      set_pv_filter { pv_name = s }) in
  let search_pv =
    H.div ~a:[H.a_class ["search"]] [
      H.div ~a:[H.a_class ["container"]] [
        H.form [input_pv]
      ]
    ]
  in

  let input_pb = input_string "problem" (fun s ->
      set_pb_filter { pb_name = s }) in
  let search_pb =
    H.div ~a:[H.a_class ["search"]] [
      H.div ~a:[H.a_class ["container"]] [
        H.form [input_pb]
      ]
    ]
  in [
    search_pv;
    search_pb;
    table;
  ]

(* Status bar *)
let st_bar =
  H.div ~a:[H.a_class ["status"]]
    [ R.Html.pcdata status ]


(* Main loop *)
let () =
  let main = Dom_html.getElementById "main" in
  let current = ref [] in
  (* Convenience function *)
  let switch f () =
    let l = List.map Tyxml_js.To_dom.of_node (f ()) in
    List.iter (fun c -> main##removeChild c |> ignore) !current;
    List.iter (fun c -> main##appendChild c |> ignore) l;
    current := l;
    Lwt.return_unit
  in
  (* Switch modes *)
  let mode_buttons =
    H.div [
      input_button "Refresh" get_snapshots;
      input_button "S-List" (switch mode_snap);
      input_button "Table" (switch mode_all);
    ]
  in
  Lwt.async
    (fun _ ->
       Lwt_js_events.domContentLoaded () >>= fun _ ->
       main##appendChild (Tyxml_js.To_dom.of_node st_bar) |> ignore;
       main##appendChild (Tyxml_js.To_dom.of_node mode_buttons) |> ignore;
       get_snapshots ()
    )

