
(** {1 Client-Side Frogweb} *)

open Lwt.Infix

module R = Tyxml_js.R
module H = Tyxml_js.Html
module L = ReactiveData.RList

type pv_filter = {
  pv_name : string;
}

type pb_filter = {
  pb_name : string;
}

(* Reactive values *)

let results, set_results =
  let l : (int * int * Run.prover Run.result) list = [] in
  React.S.create ~eq:(==) l

let pv_filter, set_pv_filter =
  let init = { pv_name = ""; } in
  React.S.create init

let pb_filter, set_pb_filter =
  let init = { pb_name = ""; } in
  React.S.create init

let pv_pre_list =
  let cmp p p' =
    let open Prover in
    compare (p.name, p.version) (p'.name, p'.version)
  in
  let aux t =
    OLinq.(of_list t
           |> map (fun (_, _, r) ->
               let `Prover p = r.Run.program in p)
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
  let proj p = p.Run.problem in
  let aux t =
    OLinq.(of_list t
           |> map (fun (_, _, r) -> r)
           |> group_by ~cmp proj
           |> sort_by ~cmp fst
           |> run_list)
  in
  React.S.map aux results

let pb_table =
  let cmp p p' =
    let open Prover in
    compare (p.name, p.version) (p'.name, p'.version)
  in
  let rec merge a b = match a, b with
    | [], [] -> []
    | (pv :: l), ((r :: l') as pbl) ->
      let `Prover pv' = r.Run.program in
      if cmp pv pv' = 0 then
        Some r :: (merge l l')
      else
        None :: (merge l pbl)
    | _ -> assert false
  in
  let aux pvs f t =
    let pred (p, _) =
      let r = Regexp.regexp_case_fold f.pb_name in
      match Regexp.string_match r p.Problem.name 0 with
      | Some _ -> true | None -> false
    in
    List.map (fun (pb, l) -> (pb, merge pvs l))
    OLinq.(of_list t
           |> filter pred
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
            | None -> H.td [ H.pcdata "." ]
            | Some r -> H.td [ res_to_html @@ Run.analyze_p r ]
          ) l)
      )) pbs
  in
  let l = L.concat th trs in
  R.Html.table l


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
       (* TODO: Load files from server *)
       main##appendChild (Tyxml_js.To_dom.of_node search_pv) |> ignore;
       main##appendChild (Tyxml_js.To_dom.of_node search_pb) |> ignore;
       main##appendChild (Tyxml_js.To_dom.of_node table) |> ignore;
       Lwt.return ()
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

