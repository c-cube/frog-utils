
module A = Archimedes
module St = FrogMapState
module Conf = FrogConfig
module Prover = FrogTPTP.Prover

(* Colors *)
exception Unknown_color

let to_color s =
  match String.lowercase s with
  | "black" -> A.Color.black
  | "red" -> A.Color.red
  | "green" -> A.Color.green
  | "blue" -> A.Color.blue
  | "yellow" -> A.Color.yellow
  | "magenta" -> A.Color.magenta
  | "cyan" -> A.Color.cyan
  | "white" -> A.Color.white
  | _ -> raise Unknown_color

let color_conv =
  let parse s =
    try
      `Ok (to_color s)
    with Unknown_color -> `Error "Not a valid color"
  in
  let print fmt c =
    let r, g, b = A.Color.get_rgb c in
    Format.fprintf fmt "rgb:%f,%f,%f" r g b
  in
  parse, print

type color_list = {
  mutable index : int;
  color_set : A.Color.t array;
}

let mk_color_list colors = {
  index = 0;
  color_set = Array.of_list colors;
}

let next_color cl =
  let c = cl.color_set.(cl.index) in
  cl.index <- (cl.index + 1) mod (Array.length cl.color_set);
  c

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
  x_axis : axis_config;
  y_axis : axis_config;
  colors : color_list;
}

let mk_graph_config x_axis y_axis style colors =
  { style; x_axis; y_axis; colors = mk_color_list colors }

type drawer = graph_config -> A.Viewport.t -> unit

let list l config v = List.iter (fun f -> f config v) l

let print_labels config v =
  A.Viewport.xlabel v config.x_axis.legend;
  A.Viewport.ylabel v config.y_axis.legend

let print_axes config v =
  A.Axes.x ~grid:config.x_axis.grid ~tics:(mk_tics config.x_axis) v;
  A.Axes.y ~grid:config.y_axis.grid ~tics:(mk_tics config.y_axis) v

let draw_on_graph config ~fmt:format ~file draw =
  let v = A.init ["Cairo"; format; file] in
  let c = A.Viewport.get_color v in
  let () = draw config v in
  A.Viewport.set_color v c;
  print_labels config v;
  print_axes config v;
  A.show v;
  A.close v

(* Misc functions *)
let fold_sum l =
  let rec aux acc sum = function
    | [] -> List.rev acc
    | x :: r -> let y = sum +. x in aux (y :: acc) y r
  in
  aux [] 0. l

(* split [l] after [n] elements *)
let ksplit n l =
  let rec aux acc k = function
    | [] -> acc, []
    | l when k <= 0 -> acc, l
    | x :: r -> aux (x :: acc) (k - 1) r
  in
  aux [] n l

let add_index l = List.mapi (fun i v -> (i, v)) l

(* TODO: comment *)
let list_filter k n l =
  let rec aux acc = function
    | [] -> List.rev acc
    | (i, x) :: r when i mod n = 0 ->
      aux ((i, x) :: acc) r
    | _ :: r -> aux acc r
  in
  let l, l' = ksplit k (List.rev l) in
  List.rev_append (aux [] l') l

(** Available drawers  *)
let draw_floats (l, name) config v =
  let n, last = match List.rev l with
    | x :: _ -> x
    | [] -> 0. , 0.
  in
  A.Viewport.set_color v (next_color config.colors);
  A.List.xy_pairs ~style:config.style v l;
  A.Viewport.text v (n +. 5.) last ~pos:A.Backend.RT name

let float_list ?(sort=false) (l, name) =
  let l = if sort then List.sort compare l else l in
  let l = List.map (fun (i, v) -> (float_of_int i, v)) (add_index l) in
  draw_floats (l, name)

let float_sum ?(filter=3) ?(count=5) ?(sort=true) (l, name) =
  let l = if sort then List.sort compare l else l in
  let l = fold_sum l in
  let l = add_index l in
  let l = list_filter count filter l in
  let l = List.map (fun (i, v) -> (float_of_int i, v)) l in
  draw_floats (l, name)

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

let graph_section = "GRAPH OPTIONS"

let axis_args axis_name dist label =
  let open Cmdliner in
  let docs = graph_section in
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
  let docs = graph_section in
  let x_axis = axis_args "x" 100. "Number of Proved Problems" in
  let y_axis = axis_args "y" 200. "Cumulative Time (in Seconds)" in
  let mark =
    let doc = "Style to use for plotting" in
    Arg.(value & opt style (`Markers "+") & info ["s"; "style"] ~doc ~docs)
  in
  let colors =
    let doc = "List of colors to use while drawing the graphs" in
    Arg.(value & opt (list color_conv) A.Color.([blue; red; green; magenta; cyan]) & info ["colors"] ~docv:"COLORS" ~doc ~docs)
  in
  Term.(pure mk_graph_config $ x_axis $ y_axis $ mark $ colors)

