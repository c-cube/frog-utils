
open Frog
module T = Test

type data =
  | Unsat_time
  | Sat_time
  | Both_time

type legend =
  | Prover

type drawer =
  | Simple of bool (* should we sort the list ? *)
  | Cumul of bool * int * int (* sort, filter, count *)

type params = {
  graph : Plot.graph_config;
  data : data;
  legend : legend;
  drawer : drawer;
  out_file : string;
  out_format : string;
}

let draw params (r:Test.top_result): Plot.drawer =
  let lazy map = r.Test.analyze in
  let datas =
    Prover.Map_name.to_list map
    |> List.map
      (fun (prover,analyze) ->
         let name = match params.legend with
           | Prover -> Prover.name prover
         and points =
           T.MStr.to_list analyze.T.Analyze.raw
           |> Misc.List.filter_map
             (fun (_file,r) ->
                let res = Event.analyze_p r in
                let ok = match res, params.data with
                  | Res.Unsat, (Unsat_time | Both_time) -> true
                  | Res.Sat, (Sat_time | Both_time) -> true
                  | _ -> false
                in
                if ok then Some r.Event.raw.Event.rtime else None)
         in
         points, name
      )
  in
  let single_drawer = match params.drawer with
    | Simple sort -> Plot.float_list ~sort
    | Cumul (sort, filter, count) -> Plot.float_sum ~sort ~filter ~count
  in
  Plot.list @@ List.map single_drawer datas

let draw_file params r =
  let d = draw params r in
  Plot.draw_on_graph params.graph ~fmt:params.out_format
    ~file:params.out_file d
