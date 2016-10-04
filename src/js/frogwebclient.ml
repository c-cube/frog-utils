
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


(* Current status *)
let status, set_status =
  React.S.create "Starting..."

(****************************************************************************)
(* Aux functions *)

let cmp_prover p p' =
  let open Prover in
  compare (p.name, p.version) (p'.name, p'.version)

(****************************************************************************)
(* HTML rendering *)

let pv_to_line p =
  let open Prover in
  match p.version with
  | Tag s ->
    [ H.pcdata (Format.sprintf "%s %s" p.name s); ]
  | Git (branch, commit) -> [
      H.pcdata (Format.sprintf "%s@@%s" p.name branch);
      H.pcdata (Format.sprintf "%s.." (String.sub commit 0 15));
    ]

let pv_to_html p =
  List.flatten (List.map (fun x -> [x; H.br ()]) (pv_to_line p))

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

let multi_choice cmp to_string list (get, set) =
  let add x =
    let old = React.S.value get in
    set (x :: old)
  in
  let rm x =
    let old = React.S.value get in
    set (List.filter (fun y -> cmp x y <> 0) old)
  in
  let is_active x =
    List.exists (Uuidm.equal x) (React.S.value get)
  in
  let tmp = React.S.map (List.map (fun x ->
      let attrs = H.a_input_type `Checkbox :: (
          if is_active x then [ H.a_checked () ] else [])
      in
      let input = H.input ~a:attrs () in
      let node = Tyxml_js.To_dom.of_input input in
      Lwt.async (fun _ ->
          Lwt_js_events.limited_loop ~elapsed_time:0.2
            Lwt_js_events.click node (fun _ _ ->
                let b = Js.to_bool ((Tyxml_js.To_dom.of_input input)##.checked) in
                if b then add x else rm x;
                Lwt.return_unit));
      (x, input), H.li [ H.label [ input; H.pcdata (to_string x) ] ]
    )) list
  in
  let s = React.S.l2 (fun l inputs ->
      (List.iter (fun ((x, input), _) ->
           let node = Tyxml_js.To_dom.of_input input in
           node##.checked := Js.bool (List.mem x l)
         ) inputs)) get tmp in
  ignore (React.S.retain get (fun () -> ignore s));
  R.Html.ul (L.map snd (L.from_signal tmp))


let on_click h k =
  let node = Tyxml_js.To_dom.of_element h in
  let () = Lwt.async (fun _ ->
      Lwt_js_events.limited_loop ~elapsed_time:0.2
        Lwt_js_events.click node (fun _ _ -> k ())) in
  h

(****************************************************************************)
(* Interace Status *)

type mode =
  | Empty
  | Table
  | List


(****************************************************************************)
(* Data & fetching *)

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
(* Interface: current state *)

(* Filters for table mode *)
let snap_filter, set_snap_filter =
  let init = [] in
  React.S.create init

let pv_filter, set_pv_filter =
  let init = { pv_name = ""; } in
  React.S.create init

let pb_filter, set_pb_filter =
  let init = { pb_name = ""; } in
  React.S.create init

(****************************************************************************)
(* Interface: snapshot list *)

let split_events =
  let rec aux acc_p acc_c = function
    | [] -> acc_p, acc_c
    | Event.Prover_run res :: r -> aux (res :: acc_p) acc_c r
    | Event.Checker_run res :: r -> aux acc_p (res :: acc_c) r
  in
  aux [] []

let stats_of_snapshot s =
  let open Event in
  let l, _ = split_events s.events in
  let provers =
    OLinq.(of_list l
           |> map (fun s -> s.program)
           |> distinct ~cmp:cmp_prover ()
           |> sort ~cmp:cmp_prover ()
           |> run_list)
  in
  let n =
    OLinq.(of_list l
           |> map (fun s -> s.problem)
           |> distinct ~cmp:compare ()
           |> size
           |> run1)
  in
  (s.uuid, s.timestamp, n, provers)

let mode_list () =
  let slist = React.S.map (fun l ->
      List.map stats_of_snapshot l
    ) snapshots in
  let table =
    let th, _ = L.create @@ [
        H.tr [
          H.td [ H.pcdata "Snapshot" ];
          H.td [ H.pcdata "Timestamp" ];
          H.td [ H.pcdata "#Problems" ];
          H.td [ H.pcdata "Provers" ];
        ]]
    in
    let trs = L.map (fun (uuid, time, n, pvs) ->
        let t = H.td [
          on_click (
            H.a ~a:[H.a_href "#table"; H.a_style "cursor:pointer" ]
              [ H.pcdata (Uuidm.to_string uuid) ])
            (fun () -> set_snap_filter [ uuid ]; Lwt.return_unit)
          ] in
        H.tr [
          t;
          H.td [ H.pcdata (Format.sprintf "%.2f" time) ];
          H.td [ H.pcdata (Format.sprintf "%d" n) ];
          H.td (List.flatten @@ List.map (fun pv -> pv_to_line pv @ [ H.br () ]) pvs);
        ]
      ) (L.from_signal slist)
    in
    R.Html.table (L.concat th trs)
  in
  [ table ]


(****************************************************************************)
(* Interface: Full table *)


let mode_table () =

  (* Initialize uuidm list *)
  let slist = React.S.map (List.map (fun s -> s.Event.uuid)) snapshots in

  (* flattened list of all events *)
  let results =
    let aux v l =
      let pred =
        if l = [] then (fun _ -> true)
        else (fun x -> List.exists (Uuidm.equal x) l)
      in
      let res, _l = List.fold_left (fun (acc, acc') s ->
          if pred s.Event.uuid then
            let l, l' = split_events s.Event.events in
            (l @ acc, l' @ acc')
          else (acc, acc')
        ) ([], []) v
      in
      res
    in
    React.S.l2 aux snapshots snap_filter
  in

  let pv_pre_list =
    let cmp = cmp_prover in
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
      let r = Regexp.regexp_case_fold f.pv_name in
      let pred p =
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
    let cmp p p' = compare (Problem.basename p) (Problem.basename p') in
    let proj p = p.Event.problem in
    let aux t =
      OLinq.(of_list t
             |> group_by ~cmp proj
             |> run_list)
    in
    React.S.map aux results
  in

  let pb_table =
    let cmp = cmp_prover in
    let merge _ l l' = match l with
      | [] -> None
      | _ -> (Some l')
    in
    let aux pvs f t =
      let pv_l = OLinq.of_list pvs in
      let r = Regexp.regexp_case_fold f.pb_name in
      let pred (p, _) =
        match Regexp.string_match r (Problem.basename p) 0 with
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
             (* |> sort_by ~cmp fst *)
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
          H.td [ H.pcdata @@ Problem.basename pb ] ::
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
    H.div ~a:[H.a_class ["table"]] [ R.Html.table l ]
  in
  (* Buttons & co *)
  let input_snap = multi_choice
    Uuidm.compare Uuidm.to_string
    slist (snap_filter, (fun l -> set_snap_filter l)) in
  let search_snap =
    H.div ~a:[H.a_class ["select"]] [
      H.pcdata "Snapshot filter";
      H.form [input_snap]
    ]
  in

  let input_pv = input_string "prover name" (fun s ->
      set_pv_filter { pv_name = s }) in
  let search_pv =
    H.div ~a:[H.a_class ["search"]] [
      H.pcdata "Prover filter";
      H.form [input_pv]
    ]
  in

  let input_pb = input_string "problem name" (fun s ->
      set_pb_filter { pb_name = s }) in
  let search_pb =
    H.div ~a:[H.a_class ["search"]] [
      H.pcdata "Problem filter";
      H.form [input_pb]
    ]
  in [
    search_snap;
    search_pv;
    search_pb;
    table;
  ]

(****************************************************************************)
(* Mode switching *)

let mode, set_mode =
  React.S.create Empty

let switch m () =
  set_mode m;
  Lwt.return_unit

let main =
  Dom_html.getElementById "main"

let current =
  React.S.map (function m ->
      List.map Tyxml_js.To_dom.of_node @@
      match m with
      | Empty -> []
      | List -> mode_list ()
      | Table -> mode_table ()
    ) mode

let _s =
  React.S.diff (fun l old ->
      List.iter (fun c -> main##removeChild c |> ignore) old;
      List.iter (fun c -> main##appendChild c |> ignore) l
    ) current

(****************************************************************************)
(* Get state from url *)

let update_state () =
  match Url.Current.get_fragment () with
  | "list" -> switch List ()
  | "table" -> switch Table ()
  | _ -> Lwt.return_unit

(****************************************************************************)
(* Status bar and main loop *)

let st_bar =
  H.div ~a:[H.a_class ["status"]]
    [ R.Html.pcdata status ]

let () =
  let nav =
    H.nav [
      H.ul [
        H.li ~a:[H.a_id "list"] [
          H.a ~a:[H.a_href "#list"] [ H.pcdata "List" ]];
        H.li ~a:[H.a_id "table"] [
          H.a ~a:[H.a_href "#table"] [ H.pcdata "Table" ]];
        on_click (
          H.li ~a:[H.a_style "float:right"] [
            H.a ~a:[] [ H.pcdata "Refresh" ]])
          get_snapshots;
      ]]
  in
  Lwt.async
    (fun _ ->
       Lwt_js_events.onhashchanges (fun _ _ -> update_state ())
    );
  Lwt.async
    (fun _ ->
       Lwt_js_events.domContentLoaded () >>= fun _ ->
       main##appendChild (Tyxml_js.To_dom.of_node nav) |> ignore;
       main##appendChild (Tyxml_js.To_dom.of_node st_bar) |> ignore;
       let%lwt () = update_state () in
       get_snapshots ()
    )

