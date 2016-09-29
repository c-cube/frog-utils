
(** {1 Client-Side Frogweb} *)

open Lwt.Infix
open Frog

module R = Tyxml_js.R
module H = Tyxml_js.Html
module L = ReactiveData.RList

type pv_filter = {
  pv_name : string;
}

type pb_filter = {
  pb_name : string;
}

(* Base Reactive values *)

let status, set_status =
  React.S.create "Starting..."

let results, set_results =
  let l : (Event.prover Event.result) list = [] in
  React.S.create ~eq:(==) l

let pv_filter, set_pv_filter =
  let init = { pv_name = ""; } in
  React.S.create init

let pb_filter, set_pb_filter =
  let init = { pb_name = ""; } in
  React.S.create init

(* Fetching snapshots *)

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

let split_events =
  let rec aux acc_p acc_c = function
    | [] -> acc_p, acc_c
    | Event.Prover_run res :: r -> aux (res :: acc_p) acc_c r
    | Event.Checker_run res :: r -> aux acc_p (res :: acc_c) r
  in
  aux [] []

let get_snapshots () =
  let%lwt frame = XmlHttpRequest.get "/snapshots/" in
  let l = snap_list frame.XmlHttpRequest.content in
  Lwt_list.iter_s (fun s ->
      let%lwt frame = XmlHttpRequest.get (Format.sprintf "/snapshot/%s" s) in
      let json = Yojson.Safe.from_string frame.XmlHttpRequest.content in
      match [%of_yojson: Event.t list] json with
      | Result.Ok l ->
        let l', _ = split_events l in
        let old = React.S.value results in
        set_results (l' @ old);
        Lwt.return_unit
      | Result.Error _ ->
        Lwt.return_unit
    ) l

(* Events updates *)

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

let pb_pre_list =
  let cmp = Problem.compare_name in
  let proj p = p.Event.problem in
  let aux t =
    OLinq.(of_list t
           |> group_by ~cmp proj
           |> run_list)
  in
  React.S.map aux results

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

(* Render the results *)

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

(* Status bar *)

let st_bar =
  H.div ~a:[H.a_class ["status"]]
    [ R.Html.pcdata status ]

(* Buttons & co *)

let input_pv =
  H.input ~a:[H.a_input_type `Text;
              H.a_placeholder "prover";]
    ()

let search_pv =
  H.div ~a:[H.a_class ["search"]] [
    H.div ~a:[H.a_class ["container"]] [
      H.form [input_pv]
    ]
  ]

let input_pb =
  H.input ~a:[H.a_input_type `Text;
              H.a_placeholder "problem";]
    ()

let search_pb =
  H.div ~a:[H.a_class ["search"]] [
    H.div ~a:[H.a_class ["container"]] [
      H.form [input_pb]
    ]
  ]

(* Main loop *)
let () =
  let main = Dom_html.getElementById "main" in
  let pv_node = Tyxml_js.To_dom.of_input input_pv in
  let pb_node = Tyxml_js.To_dom.of_input input_pb in
  Lwt.async
    (fun _ ->
       Lwt_js_events.domContentLoaded () >>= fun _ ->
       main##appendChild (Tyxml_js.To_dom.of_node st_bar) |> ignore;
       main##appendChild (Tyxml_js.To_dom.of_node search_pv) |> ignore;
       main##appendChild (Tyxml_js.To_dom.of_node search_pb) |> ignore;
       main##appendChild (Tyxml_js.To_dom.of_node table) |> ignore;
       get_snapshots ()
  );

  Lwt.async (fun _ ->
      Lwt_js_events.limited_loop ~elapsed_time:0.2
        Lwt_js_events.input pv_node (fun _ _ ->
            let pv_name = Js.to_string ((Tyxml_js.To_dom.of_input input_pv)##.value) in
            set_pv_filter { pv_name; };
            Lwt.return ()));
  Lwt.async (fun _ ->
      Lwt_js_events.limited_loop ~elapsed_time:0.2
        Lwt_js_events.input pb_node (fun _ _ ->
            let pb_name = Js.to_string ((Tyxml_js.To_dom.of_input input_pb)##.value) in
            set_pb_filter { pb_name; };
            Lwt.return ()))

