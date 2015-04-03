
module A = Archimedes
module St = FrogMapState
module Conf = FrogConfig
module Prover = FrogTPTP.Prover

module Opt = struct
  let (>>=) o f = match o with
    | None -> None
    | Some x -> f x
end

(* Options for drawing graph *)
type axis_config = {
  show : bool;
  grid : bool;
  legend : string;
  label_marks : int;
  label_dist : float;
  label_start : float;
}

let mk_axis_config show grid legend
    label_marks label_dist label_start =
  { show; grid; legend; label_marks; label_dist; label_start; }

let mk_tics axis =
  A.Tics.Equidistants (
    A.Tics.Number 10,
    axis.label_start, axis.label_dist, axis.label_marks
  )

type graph_config = {
  style : [ `Lines | `Linesmarkers of string | `Markers of string ];
  count : int;
  filter : int;
  x_axis : axis_config;
  y_axis : axis_config;
}

let mk_graph_config x_axis y_axis style count filter =
  { style; count; filter; x_axis; y_axis; }

(* Misc functions *)
let fold_sum l =
  let rec aux acc sum = function
    | [] -> List.rev acc
    | x :: r -> let y = sum +. x in aux (y :: acc) y r
  in
  aux [] 0. l

let ksplit n l =
  let rec aux acc k = function
    | [] -> acc, []
    | l when k <= 0 -> acc, l
    | x :: r -> aux (x :: acc) (k - 1) r
  in
  aux [] n l

let add_index l = List.mapi (fun i v -> (i, v)) l

let filter k n l =
  let rec aux acc = function
    | [] -> List.rev acc
    | (k, x) :: r when k mod n = 0 ->
      aux ((k, x) :: acc) r
    | _ :: r -> aux acc r
  in
  let l, l' = ksplit k (List.rev l) in
  List.rev_append (aux [] l') l

(** Plotting functions *)
let print_time config v (l, name) =
  let l = add_index (fold_sum (List.sort compare l)) in
  let l = List.map (fun (i, v) -> (float_of_int i, v)) (filter config.count config.filter l) in
  let n, last =
    try
      List.hd (List.rev l)
    with Failure "hd" ->
      0., 0.
  in
  A.List.xy_pairs ~style:config.style v l;
  A.Viewport.text v (n +. 5.) last ~pos:A.Backend.RT name

let print_labels config v =
  A.Viewport.xlabel v config.x_axis.legend;
  A.Viewport.ylabel v config.y_axis.legend

let print_axes config v =
  A.Axes.x ~grid:config.x_axis.grid ~tics:(mk_tics config.x_axis) v;
  A.Axes.y ~grid:config.y_axis.grid ~tics:(mk_tics config.y_axis) v

let draw_graph config format filename args =
  let v = A.init ["Cairo"; format; filename] in
  A.Viewport.set_color v A.Color.blue;
  List.iter (print_time config v) args;
  A.Viewport.set_color v A.Color.black;
  print_labels config v;
  print_axes config v;
  A.show v;
  A.close v

(** Analysing functions *)
let compile_re ~msg re =
  try
    Some (Re.compile (Re.no_case (Re_posix.re re)))
  with e ->
    Printf.eprintf "could not compile regex %s: %s"
      msg (Printexc.to_string e);
    None

let matches maybe_re s = match maybe_re with
  | None -> false
  | Some re -> Re.execp re s

let unsat_times (prover, file) =
  let re_unsat = Opt.(prover.Prover.unsat >>= compile_re ~msg:"unsat") in
  St.fold_state
    (fun acc { St.res_time = t; St.res_out = out; _ } ->
       if matches re_unsat out then t :: acc else acc)
    (fun _ -> []) file

(** Commands *)
let main graph_config config_files format out args =
  let config = Conf.parse_files (Conf.interpolate_home "$HOME/.frogtptp.toml" :: config_files) Conf.empty in
  let provers, names = List.split (List.map (fun (s, f) -> (Prover.find_config config s, f), s) args) in
  let times = Lwt_main.run (Lwt_list.map_s unsat_times provers) in
  let l = List.combine times names in
  draw_graph graph_config format out l

let config_file =
  let parse s =
    let f = Conf.interpolate_home s in
    if Sys.file_exists f then
      if not (Sys.is_directory f) then
        `Ok f
      else
        `Error (f ^ " is a directory, not a file")
    else
      `Error (f ^ " is not a valid file")
  in
  let print fmt s = Format.fprintf fmt "%s" s in
  parse, print

let style =
  let parse s =
    match Re.split (Re.compile (Re.char ',')) s with
    | ["lines"] -> `Ok (`Lines)
    | ["markers"; m] -> `Ok (`Markers m)
    | _ -> `Error "Wrong style formatting"
  in
  let print fmt = function
    | `Lines -> Format.fprintf fmt "Lines"
    | `Linesmarkers s -> Format.fprintf fmt "lines markers %s" s
    | `Markers s -> Format.fprintf fmt "markers %s" s
  in
  parse, print

let axis_sect = "AXIS OPTIONS"
let common_sect = "COMMON OPTIONS"

let axis_args axis_name dist label =
  let open Cmdliner in
  let docs = axis_sect in
  let oname s = axis_name ^ "." ^ s in
  let show =
    let doc = "Wether to show draw the axis" in
    Arg.(value & opt bool true & info [oname "show"] ~docs ~doc)
  in
  let grid =
    let doc = "Wether to draw the grid for the axis" in
    Arg.(value & opt bool true & info [oname "grid"] ~docs ~doc)
  in
  let legend =
    let doc = "Legend of the axis" in
    Arg.(value & opt string label & info [oname "legend"] ~docs ~doc)
  in
  let label_marks =
    let doc = "Number of marks between two axis labels" in
    Arg.(value & opt int 1 & info [oname "lmarks"] ~docs ~doc)
  in
  let label_dist =
    let doc = "Distance between two axis labels" in
    Arg.(value & opt float dist & info [oname "ldist"] ~docs ~doc)
  in
  let label_start =
    let doc = "Start of axis labels" in
    Arg.(value & opt float 0. & info [oname "lstart"] ~docs ~doc)
  in
  Term.(pure mk_axis_config $ show $ grid $ legend $
        label_marks $ label_dist $ label_start)

let graph_args =
  let open Cmdliner in
  let docs = common_sect in
  let x_axis = axis_args "x" 100. "Number of problems proved" in
  let y_axis = axis_args "y" 200. "Cumulative time (in seconds)" in
  let mark =
    let doc = "Style to use for plotting" in
    Arg.(value & opt style (`Markers "+") & info ["s"; "style"] ~docs ~doc)
  in
  let filter =
    let doc = "Set if you want to filter the list of points drawed. One in every $(docv) point is drawn." in
    Arg.(value & opt int 3 & info ["f"; "filter"] ~docs ~docv:"N" ~doc)
  in
  let last =
    let doc = "Prevents the last $(docv) points from begin filtered by the 'filter' option" in
    Arg.(value & opt int 10 & info ["l"; "last"] ~docv:"N" ~docs ~doc)
  in
  Term.(pure mk_graph_config $ x_axis $ y_axis $ mark $ last $ filter)

let main () =
  let open Cmdliner in
  let docs = common_sect in
  let config =
    let doc = "Config file" in
    Arg.(value & opt_all config_file [] & info ["c"; "config"] ~docv:"FILE" ~docs ~doc)
  in
  let args =
    let doc = "List of pairs of provers and result files." in
    Arg.(non_empty & pos_all (pair string non_dir_file) [] & info [] ~docs ~doc)
  in
  let format =
    let doc = "Format of the output file" in
    Arg.(value & opt string "PNG" & info ["f"; "format"] ~docs ~doc)
  in
  let out =
    let doc = "Output file" in
    Arg.(required & opt (some string) None & info ["o"; "out"] ~docs ~doc)
  in
  let man = [
    `S common_sect; `P "Common options for frogplot.";
    `S axis_sect; `P "Options for the drawing og axes.";
  ] in
  let t_info = Term.info ~man "frogplot" in
  match Term.(eval (pure main $ graph_args $ config $ format $ out $ args, t_info)) with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> raise Exit
  | `Ok () -> ()
;;

try
  main ()
with
| Exit -> exit 2

