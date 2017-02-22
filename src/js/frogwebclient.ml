
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

type expect =
  | Same
  | Better
  | Mismatch
  | Compatible

type ev_filter = {
  res : Res.t list;
  expect : expect list;
}

(****************************************************************************)
(* Interface: current state *)

(* Current status *)
let status, set_status =
  React.S.create "Starting..."

(* Filters for table mode *)
let s_filter, set_s_filter =
  let init = [] in
  React.S.create init

let pv_filter, set_pv_filter =
  let init = { pv_name = ""; } in
  React.S.create init

let pb_filter, set_pb_filter =
  let init = { pb_name = ""; } in
  React.S.create init

let ev_filter, set_ev_filter =
  let init = { res = [] ; expect = [] } in
  React.S.create init

(* Single event printing *)
let event, set_event =
  React.S.create None

let prover, set_prover =
  React.S.create None

let problem, set_problem =
  React.S.create None

let pb_contents, set_pb_contents =
  React.S.create None

let _update_pb_contents =
  React.S.map (function
      | None -> ()
      | Some f ->
        Lwt.async (
          function () ->
            let%lwt frame = XmlHttpRequest.get
                (Format.sprintf "/problem/?file=%s" f.Problem.name) in
            if frame.XmlHttpRequest.code = 404 then
              set_pb_contents (Some "Error 404")
            else
              set_pb_contents (Some frame.XmlHttpRequest.content);
            Lwt.return_unit
        )
    ) problem

(****************************************************************************)
(* Aux functions *)

let cmp_res r r' =
  match Res.compare r r' with
  | `Same -> Same
  | `Mismatch -> Mismatch
  | `LeftBetter -> Better
  | `RightBetter -> Compatible

let expect_to_string = function
  | Same -> "correct"
  | Better -> "better"
  | Mismatch -> "mismatch"
  | Compatible -> "compatible"

let mem ?(cmp=compare) x = function
  | [] -> true
  | l -> List.exists (fun y -> cmp x y = 0) l

let cmp_prover p p' =
  let open Prover in
  compare (p.name, p.version) (p'.name, p'.version)

let filter_s =
  React.S.map ~eq:(==) (function
      | [] -> (fun _ -> true)
      | l -> (fun s ->
          List.exists (Uuidm.equal s.Event.uuid) l)
    ) s_filter

let filter_pv =
  React.S.map ~eq:(==) (function { pv_name; } ->
      let r = Regexp.regexp_case_fold pv_name in
      (fun p ->
         match Regexp.string_match r p.Prover.name 0 with
         | Some _ -> true | None -> false)
    ) pv_filter

let filter_pb =
  React.S.map ~eq:(==) (function { pb_name; } ->
      let r = Regexp.regexp_case_fold pb_name in
      (fun p ->
         match Regexp.string_match r (Problem.basename p) 0 with
         | Some _ -> true | None -> false)
    ) pb_filter

let filter_ev =
  React.S.map ~eq:(==) (function { res; expect; } ->
      (function
        | Event.Prover_run result ->
          let r = Event.analyze_p result in
          mem r res && mem (cmp_res r result.Event.problem.Problem.expected) expect
        | Event.Checker_run _ -> true
      )
    ) ev_filter

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
  let tmp = React.S.map (List.map (fun x ->
      let input = H.input ~a:[H.a_input_type `Checkbox] () in
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
           node##.checked := Js.bool (List.exists (fun y -> cmp x y = 0) l)
         ) inputs)) get tmp in
  ignore (React.S.retain get (fun () -> ignore s));
  H.div ~a:[H.a_class ["choice"]]
    [ R.Html.ul (L.map snd (L.from_signal tmp)) ]


let on_click h k =
  let node = Tyxml_js.To_dom.of_element h in
  let () = Lwt.async (fun _ ->
      Lwt_js_events.limited_loop ~elapsed_time:0.2
        Lwt_js_events.click node (fun _ _ -> k ())) in
  h

(****************************************************************************)
(* HTML rendering *)

let pb_to_html pb =
  on_click
    (H.a ~a:[H.a_href "#problem"]
       [ H.pcdata (Problem.basename pb) ])
    (function () -> set_problem (Some pb); Lwt.return_unit)

let version_to_string = function
  | Prover.Tag s -> Format.sprintf " %s" s
  | Prover.Git (branch, commit) ->
    Format.sprintf "@@%s#%s…" branch (String.sub commit 0 15)

let pv_to_line p =
  let open Prover in
  [ on_click
      (H.a ~a:[H.a_href "#prover"]
         [ H.pcdata (Format.sprintf "%s%s" p.name (version_to_string p.version)) ])
      (fun () -> set_prover (Some p); Lwt.return_unit);
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
  H.span ~a:[H.a_class ["result"];
             H.a_style ("color:"^color)]
    [ H.pcdata (to_string r) ]

let prover_run_to_html r =
  H.span [
    on_click (
      H.a ~a:[H.a_href "#event"]
        [res_to_html @@ Event.analyze_p r])
      (fun () -> set_event (Some (Event.Prover_run r)); Lwt.return_unit);
    H.br ();
  ]

let int_to_html i =
  H.pcdata (Format.sprintf "%d" i)

let float_to_html f =
  H.pcdata (Format.sprintf "%.2f" f)

let pre s =
  H.pre ~a:[H.a_class [ "raw" ] ] [ H.pcdata s ]

let pre_opt default = function
  | None -> H.pcdata default
  | Some s -> pre s

(****************************************************************************)
(* Data & fetching *)

let (snapshots_meta:Event.Meta.t list React.signal), set_snapshots_meta =
  React.S.create []

let (snapshots:Event.Snapshot.t list React.signal), set_snapshots =
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

let get_snapshots_meta () =
  set_snapshots_meta [];
  let%lwt frame = XmlHttpRequest.get "/snapshots/" in
  let l = snap_list frame.XmlHttpRequest.content in
  let n = List.length l in
  let%lwt () = Lwt_list.iteri_s (fun i s ->
      set_status (Format.sprintf "Fetching metadata snapshot %d/%d ..." (i + 1) n);
      let%lwt frame = XmlHttpRequest.get (Format.sprintf "/snapshot/meta/%s" s) in
      set_status (Format.sprintf "Parsing snapshot %d/%d ..." (i + 1) n);
      let json = Yojson.Safe.from_string frame.XmlHttpRequest.content in
      match [%of_yojson: Event.Meta.t] json with
      | Result.Ok s ->
        set_status (Format.sprintf "Adding snapshot metadata %d/%d ..." (i + 1) n);
        let old = React.S.value snapshots_meta in
        set_snapshots_meta (s :: old);
        Lwt.return_unit
      | Result.Error _ ->
        set_status (Format.sprintf "Error while reading snapshot metadata %d" (i + 1));
        Lwt.return_unit
    ) l in
  Lwt.return_unit


(****************************************************************************)
(* Interface: snapshot list *)

let split_events =
  let rec aux acc_p acc_c = function
    | [] -> acc_p, acc_c
    | Event.Prover_run res :: r -> aux (res :: acc_p) acc_c r
    | Event.Checker_run res :: r -> aux acc_p (res :: acc_c) r
  in
  aux [] []

let stats_of_snapshot_meta (s:Event.Meta.t) =
  let provers = Event.Meta.provers s |> Prover.Set.elements in
  Event.Meta.uuid s, Event.Meta.timestamp s, Event.Meta.length s, provers

let date_to_string (t:float): string =
  let module T = ISO8601.Permissive in
  T.string_of_datetime t

let mode_list () =
  (* list of snapshots, sorted by decreasing timestamps *)
  let slist = React.S.map (fun l ->
      List.map stats_of_snapshot_meta l
      |> List.sort (fun (_,t1,_,_)(_,t2,_,_) -> compare t2 t1)
    ) snapshots_meta in
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
            (fun () -> set_s_filter [ uuid ]; Lwt.return_unit)
          ] in
        H.tr [
          t;
          H.td [ H.pcdata (date_to_string time) ];
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

(* FIXME: need to load the snapshots selected in list *)

let mode_table () =

  (* Initialize uuidm list *)
  let ulist = React.S.map (List.map Event.Meta.uuid) snapshots_meta in

  (* flattened list of all events *)
  let snap_list =
    React.S.l2 List.filter filter_s snapshots
  in

  let results =
    let aux pred l =
      let res, _l = List.fold_left (fun (acc, acc') s ->
          let evs = List.filter pred s.Event.events in
          let l, l' = split_events evs in
          (l @ acc, l' @ acc')) ([], []) l
      in
      res
    in
    React.S.l2 aux filter_ev snap_list
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
    React.S.l2 List.filter filter_pv pv_pre_list
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
    let aux pvs pred t =
      let pv_l = OLinq.of_list pvs in
      OLinq.(of_list t
             |> filter (fun (pb, _) -> pred pb)
             |> map (fun (pb, l) ->
                 let l' =
                   of_list l
                   |> outer_join ~cmp
                     (fun x -> x)
                     (fun r -> r.Event.program)
                     ~merge pv_l
                   |> run_list
                 in pb, l')
             (* |> sort_by ~cmp fst *)
             |> run_list)
    in
    React.S.l3 aux pv_list filter_pb pb_pre_list
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
          H.td [ pb_to_html pb ] ::
          List.map (
            function
            | [] -> H.td [ H.pcdata "." ]
            | l -> H.td (List.map prover_run_to_html l)
          ) l)
      ) pbs
    in
    let l = L.concat th trs in
    H.div ~a:[H.a_class ["table"]] [ R.Html.table l ]
  in
  (* Buttons & co *)
  let input_snap = multi_choice
    Uuidm.compare Uuidm.to_string
    ulist (s_filter, (fun l -> set_s_filter l)) in
  let search_snap =
    H.div ~a:[H.a_class ["select"]] [
      H.h3 [ H.pcdata "Snapshots" ];
      H.div~a:[H.a_class ["searchbox"]] [
        H.h5 [ H.pcdata "List" ];
        input_snap;
      ];
    ]
  in

  let input_pv = input_string "prover name" (fun s ->
      set_pv_filter { pv_name = s }) in
  let input_pb = input_string "problem name" (fun s ->
      set_pb_filter { pb_name = s }) in
  let input_res = multi_choice
      compare Res.to_string
      (React.S.const [Res.Sat;Res.Unsat;Res.Unknown;Res.Timeout;Res.Error])
      ((React.S.map (fun { res; _ } -> res) ev_filter),
       (fun l -> set_ev_filter { (React.S.value ev_filter) with res = l; }))
  in
  let input_expect = multi_choice
      compare expect_to_string
      (React.S.const [Same; Better; Mismatch; Compatible])
      ((React.S.map (fun { expect; _ } -> expect) ev_filter),
       (fun l -> set_ev_filter { (React.S.value ev_filter) with expect = l; }))
  in
  let filter =
    H.div ~a:[H.a_class ["search"]] [
      H.h3 [ H.pcdata "Filter" ];
      H.form ~a:[H.a_class ["searchbox"]] [
        H.h5 [ H.pcdata "Prover&Problem" ];
        input_pv;
        H.br ();
        input_pb;
      ];
      H.div ~a:[H.a_class ["searchbox"]] [
        H.h5 [ H.pcdata "Result" ];
        input_res;
      ];
      H.div ~a:[H.a_class ["searchbox"]] [
        H.h5 [ H.pcdata "Expect" ];
        input_expect;
      ];
    ] in
  [
    search_snap;
    filter;
    table;
  ]

(****************************************************************************)
(* Interface: Result printing *)

let mode_event () =
  let l = React.S.map (function
      | None ->
        [ H.pcdata "Please select an event to print " ]
      | Some Event.Checker_run _ ->
        [ H.pcdata "TODO (checker events)" ]
      | Some Event.Prover_run r ->
        [ H.table
            [ H.tr [ H.td [ H.pcdata "Prover" ];
                     H.td (pv_to_html r.Event.program) ];
              H.tr [ H.td [ H.pcdata "Problem" ];
                     H.td [ pb_to_html r.Event.problem ] ];
              H.tr [ H.td [ H.pcdata "Result" ];
                     H.td [ res_to_html (Event.analyze_p r) ] ];
              H.tr [ H.td [ H.pcdata "Errcode" ];
                     H.td [ int_to_html r.Event.raw.Event.errcode ] ];
              H.tr [ H.td [ H.pcdata "real time" ];
                     H.td [ float_to_html r.Event.raw.Event.rtime ] ];
              H.tr [ H.td [ H.pcdata "user time" ];
                     H.td [ float_to_html r.Event.raw.Event.utime ] ];
              H.tr [ H.td [ H.pcdata "system time" ];
                     H.td [ float_to_html r.Event.raw.Event.stime ] ];
              H.tr [ H.td [ H.pcdata "stdout" ];
                     H.td [ pre r.Event.raw.Event.stdout ] ];
              H.tr [ H.td [ H.pcdata "stderr" ];
                     H.td [ pre r.Event.raw.Event.stderr ] ];
            ]
        ]
    ) event
  in
  [ R.Html.div (L.from_signal l) ]

(****************************************************************************)
(* Interface: Prover printing *)

let mode_prover () =
  let l = React.S.map (function
      | None ->
        [ H.pcdata "Please select a prover to print" ]
      | Some p ->
        [ H.h3 (pv_to_line p);
          H.table
            [ H.tr [ H.td [ H.pcdata "version" ];
                     H.td [ H.pcdata (version_to_string p.Prover.version) ] ];
              H.tr [ H.td [ H.pcdata "binary" ];
                     H.td [ H.pcdata p.Prover.binary ] ];
              H.tr [ H.td [ H.pcdata "cmd" ];
                     H.td [ pre p.Prover.cmd ] ];
              H.tr [ H.td [ H.pcdata "sat" ];
                     H.td [ pre_opt "<none>" p.Prover.sat ] ];
              H.tr [ H.td [ H.pcdata "unsat" ];
                     H.td [ pre_opt "<none>" p.Prover.unsat ] ];
              H.tr [ H.td [ H.pcdata "unknown" ];
                     H.td [ pre_opt "<none>" p.Prover.unknown ] ];
              H.tr [ H.td [ H.pcdata "timeout" ];
                     H.td [ pre_opt "<none>" p.Prover.timeout ] ];
              H.tr [ H.td [ H.pcdata "out of space" ];
                     H.td [ pre_opt "<none>" p.Prover.memory ] ];
            ]
        ]
    ) prover
  in
  [ R.Html.div (L.from_signal l) ]

(****************************************************************************)
(* Interface: Problem printing *)

let mode_problem () =
  let l = React.S.l2 (fun opt contents ->
      match opt with
      | None ->
        [ H.pcdata "Please select a problem to be printed" ];
      | Some pb ->
        [ H.h3 [ H.pcdata (Problem.basename pb) ];
          H.table
            [ H.tr [ H.td [ H.pcdata "Path" ];
                     H.td [ H.pcdata pb.Problem.name ] ];
              H.tr [ H.td [ H.pcdata "Expected" ];
                     H.td [ res_to_html pb.Problem.expected ] ];
              H.tr [ H.td [ H.pcdata "Contents" ];
                     H.td [ pre_opt "fetching data..." contents ] ];
            ]
        ]) problem pb_contents in
  [ R.Html.div (L.from_signal l) ]

(****************************************************************************)
(* Mode switching *)

type mode =
  | Empty
  | Table
  | List
  | Event
  | Prover
  | Problem

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
      | Event -> mode_event ()
      | Prover -> mode_prover ()
      | Problem -> mode_problem ()
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
  | "event" -> switch Event ()
  | "prover" -> switch Prover ()
  | "problem" -> switch Problem ()
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
        H.li ~a:[H.a_id "event"] [
          H.a ~a:[H.a_href "#event"] [ H.pcdata "Event" ]];
        H.li ~a:[H.a_id "prover"] [
          H.a ~a:[H.a_href "#prover"] [ H.pcdata "Prover" ]];
        H.li ~a:[H.a_id "problem"] [
          H.a ~a:[H.a_href "#problem"] [ H.pcdata "Problem" ]];
        on_click (
          H.li ~a:[H.a_style "float:right"] [
            H.a ~a:[] [ H.pcdata "Refresh" ]])
          get_snapshots_meta;
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
       (* load metadata *)
       get_snapshots_meta ()
    )

