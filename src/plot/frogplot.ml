
open Result
open Frog
open Frog_server

module T = Test
module E = Misc.LwtErr

module Plot_run = struct
  (* Plot functions *)
  let main ~config params (name:string option) : unit E.t =
    let open E in
    let storage = Storage.make [] in
    Test_run.find_or_last ~storage name >>= fun main_res ->
    Test_plot.draw_file params main_res;
    E.return ()
end

let config_term =
  let open Cmdliner in
  let aux config debug =
    if debug then (
      Maki_log.set_level 5;
      Lwt_log.add_rule "*" Lwt_log.Debug;
    );
    let config = Config.interpolate_home config in
    try
      `Ok (Config.parse_files [config] Config.empty)
    with Config.Error msg ->
      `Error (false, msg)
  in
  let arg =
    Arg.(value & opt string "$home/.frogutils.toml" &
         info ["c"; "config"] ~doc:"configuration file (in target directory)")
  and debug =
    let doc = "Enable debug (verbose) output" in
    Arg.(value & flag & info ["d"; "debug"] ~doc)
  in
  Term.(ret (pure aux $ arg $ debug))

let snapshot_name_term : string option Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(value & pos 0 (some string) None
       & info [] ~docv:"FILE" ~doc:"file/name containing results (default: last)")

let drawer_term =
  let open Cmdliner in
  let open Test_plot in
  let aux cumul sort filter count =
    if cumul then Cumul (sort, filter, count) else Simple sort
  in
  let cumul =
    let doc = "Plots the cumulative sum of the data" in
    Arg.(value & opt bool true & info ["cumul"] ~doc)
  in
  let sort =
    let doc = "Should the data be sorted before being plotted" in
    Arg.(value & opt bool true & info ["sort"] ~doc)
  in
  let filter =
    let doc = "Plots one in every $(docv) data point
              (ignored if not in cumulative plotting)" in
    Arg.(value & opt int 3 & info ["pspace"] ~doc)
  in
  let count =
    let doc = "Plots the last $(docv) data point in any case" in
    Arg.(value & opt int 5 & info ["count"] ~doc)
  in
  Term.(pure aux $ cumul $ sort $ filter $ count)

let plot_params_term =
  let open Cmdliner in
  let open Test_plot in
  let aux graph data legend drawer out_file out_format =
    { graph; data; legend; drawer; out_file; out_format }
  in
  let to_cmd_arg l = Cmdliner.Arg.enum l, Cmdliner.Arg.doc_alts_enum l in
  let data_conv, data_help = to_cmd_arg
      [ "unsat_time", Unsat_time; "sat_time", Sat_time; "both_time", Both_time ] in
  let legend_conv, legend_help = to_cmd_arg [ "prover", Prover ] in
  let data =
    let doc = Format.sprintf "Decides which value to plot. $(docv) must be %s" data_help in
    Arg.(value & opt data_conv Both_time & info ["data"] ~doc)
  and legend =
    let doc = Format.sprintf
        "What legend to attach to each curve. $(docv) must be %s" legend_help
    in
    Arg.(value & opt legend_conv Prover & info ["legend"] ~doc)
  and out_file =
    let doc = "Output file for the plot" in
    Arg.(required & opt (some string) None & info ["o"; "out"] ~doc)
  and out_format =
    let doc = "Output format for the graph" in
    Arg.(value & opt string "PDF" & info ["format"] ~doc)
  in
  Term.(pure aux $ Plot.graph_args $ data $ legend $ drawer_term $ out_file $ out_format)

let term_plot =
  let open Cmdliner in
  let aux config params file = Lwt_main.run (Plot_run.main ~config params file) in
  let doc = "Plot graphs of prover's statistics" in
  let man = [
    `S "DESCRIPTION";
    `P "This tools takes results files from runs of '$(b,frogmap)' and plots graphs
        about the prover's statistics.";
    `S "OPTIONS";
    `S Plot.graph_section;
  ] in
  Term.(pure aux $ config_term $ plot_params_term $ snapshot_name_term),
  Term.info ~man ~doc "plot"

let parse_opt () =
  let open Cmdliner in
  Cmdliner.Term.eval term_plot

let () =
  match parse_opt () with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok (Ok ()) -> ()
  | `Ok (Error e) ->
      print_endline ("error: " ^ e);
      exit 1
