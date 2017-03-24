
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(* run tests, or compare results *)

open Result
open Frog
open Frog_server
module T = Test
module E = Misc.LwtErr

(** {2 Run} *)
module Run = struct
  (* callback that prints a result *)
  let nb_sec_minute = 60
  let nb_sec_hour = 60 * nb_sec_minute
  let nb_sec_day = 24 * nb_sec_hour

  let time_string f =
    let n = int_of_float f in
    let aux n div = n / div, n mod div in
    let n_day, n = aux n nb_sec_day in
    let n_hour, n = aux n nb_sec_hour in
    let n_min, n = aux n nb_sec_minute in
    let print_aux s n = if n <> 0 then (string_of_int n) ^ s else "" in
    (print_aux "d" n_day) ^
    (print_aux "h" n_hour) ^
    (print_aux "m" n_min) ^
    (string_of_int n) ^ "s"

  let progress_dynamic len =
    let start = Unix.gettimeofday () in
    let count = ref 0 in
    function res ->
      let time_elapsed = Unix.gettimeofday () -. start in
      incr count;
      let len_bar = 50 in
      let bar = String.init len_bar
          (fun i -> if i * len <= len_bar * !count then '#' else '-') in
      let percent = if len=0 then 100 else (!count * 100) / len in
      Format.printf "... %5d/%d | %3d%% [%6s: %s]@?"
        !count len percent (time_string time_elapsed) bar;
      if !count = len then Format.printf "@."

  let progress ?(dyn=false) n =
    let pp_bar = progress_dynamic n in
    (function res ->
       if dyn then Format.printf "\r";
       Test_run.print_result res;
       if dyn then pp_bar res;
       Lwt.return_unit)

  (* run provers on the given dir, return a list [prover, dir, results] *)
  let test_dir ?dyn ~with_lock ~ipc ?j ?timeout ?memory ?caching ?provers ~config d
    : T.Top_result.t E.t =
    let open E.Infix in
    let dir = d.T.Config.directory in
    begin
      Format.printf "testing dir `%s`...@." dir;
      Problem_run.of_dir dir
        ~filter:(Re.execp (Re_posix.compile_pat d.T.Config.pattern)) |> E.ok
      >>= fun pbs ->
      let len = List.length pbs in
      Format.printf "run %d tests in %s@." len dir;
      let provers = match provers with
        | None -> config.T.Config.provers
        | Some l ->
          List.filter
            (fun p -> List.mem (Prover.name p) l)
            config.T.Config.provers
      in
      let%lwt () =
        IPC_client.send ipc (IPC_message.Start_bench (len * List.length provers))
      in
      let on_solve =
        let prog = progress ?dyn (len * List.length provers) in
        fun r ->
          let%lwt () = prog r in
          IPC_client.send ipc (IPC_message.Event (Event.Prover_run r))
      in
      (* solve *)
      let main =
        Test_run.run ?j ?timeout ?memory ?caching ~provers
          ?lock:(if with_lock then Some ipc else None)
          ~expect:d.T.Config.expect ~on_solve ~config pbs
        |> E.add_ctxf "running %d tests" len
      in
      main
      >>= fun results ->
      let%lwt () = IPC_client.send ipc IPC_message.Finish_bench in
      Prover.Map_name.iter
        (fun p r ->
           Format.printf "@[<2>%s on `%s`:@ @[<hv>%a@]@]@."
             (Prover.name p) dir T.Analyze.print r)
        (Lazy.force results.T.analyze);
      E.return results
    end |> E.add_ctxf "running tests in dir `%s`" dir

  let check_res (results:T.top_result) : unit E.t =
    let lazy map = results.T.analyze in
    if Prover.Map_name.for_all (fun _ r -> T.Analyze.is_ok r) map
    then E.return ()
    else
      E.failf "%d failure(s)"
        (Prover.Map_name.fold (fun _ r n -> n + T.Analyze.num_failed r) map
            0)

  (* lwt main *)
  let main ?dyn ~port ?j ?timeout ?memory ?caching ?junit ?provers ?meta ~with_lock ~save ~config ?dir_file dirs () =
    let open E.Infix in
    (* parse list of files, if need be *)
    let%lwt dirs = match dir_file with
      | None -> Lwt.return dirs
      | Some f ->
        let%lwt f_lines =
          Lwt_io.with_file ~mode:Lwt_io.input f
            (fun ic -> Lwt_io.read_lines ic |> Lwt_stream.to_list)
        in
        Lwt.return (List.rev_append f_lines dirs)
    in
    (* parse config *)
    begin
      Lwt.return (Test_run.config_of_config config dirs)
      |> E.add_ctxf "parsing config from [@[%a@]]" (Misc.Fmt.pp_list Format.pp_print_string) dirs
    end
    >>= fun config ->
    (* pick default directory if needed *)
    let problems = config.T.Config.problems in
    let storage = Storage.make [] in
    (* build problem set (exclude config file!) *)
    let task_with_conn c =
      E.map_s
        (test_dir ?dyn ~with_lock ~ipc:c ?j ?timeout ?memory ?caching ?provers ~config)
        problems
    in
    IPC_client.connect_or_spawn port task_with_conn
    >|= T.Top_result.merge_l
    >>= fun (results:T.Top_result.t) ->
    begin match save with
      | "none" -> E.return ()
      | "" ->
        (* default *)
        let snapshot = Event.Snapshot.make ?meta results.T.events in
        let uuid_s = Uuidm.to_string snapshot.Event.uuid in
        let%lwt () = Lwt_io.printlf "save with UUID `%s`" uuid_s in
        Storage.save_json storage uuid_s (Event.Snapshot.to_yojson snapshot)
        |> E.ok
      | file ->
        T.Top_result.to_file ~file results
    end >>= fun () ->
    begin match junit with
      | None -> ()
      | Some file ->
        Lwt_log.ign_info_f "write results in Junit to file `%s`" file;
        let suites =
          Lazy.force results.T.analyze
          |> Prover.Map_name.to_list
          |> List.map (fun (_,a) -> JUnit_wrapper.test_analyze a) in
        JUnit_wrapper.junit_to_file suites file;
    end;
    (* now fail if results were bad *)
    check_res results
end

(** {2 Display Run} *)
module Display = struct
  let main (name:string option)(provers:string list option)(dir:string list) =
    let open E in
    let storage = Storage.make [] in
    Test_run.find_or_last ~storage name >>= fun res ->
    let res = T.Top_result.filter ~provers ~dir res in
    Format.printf "%a@." T.Top_result.pp res;
    E.return ()
end

(** {2 CSV Run} *)
module CSV = struct
  let main (file:string option) (out:string option) =
    let open E in
    let storage = Storage.make [] in
    Test_run.find_or_last ~storage file >>= fun res ->
    begin match out with
      | None ->
        print_endline (T.Top_result.to_csv_string res)
      | Some file ->
        T.Top_result.to_csv_file file res
    end;
    E.return ()
end

(** {2 Compare Run} *)
module Compare = struct
  let main ~file1 ~file2 () =
    let open E in
    let storage = Storage.make [] in
    Test_run.find_results ~storage file1 >>= fun res1 ->
    Test_run.find_results ~storage file2 >>= fun res2 ->
    let cmp = T.Top_result.compare res1 res2 in
    Format.printf "%a@." T.Top_result.pp_comparison cmp;
    E.return ()
end

(** {2 Display+ Compare} *)
module Display_bench = struct
  let main (name:string option)(provers:string list option)(dir:string list) =
    let open E in
    let storage = Storage.make [] in
    Test_run.find_or_last ~storage name >>= fun res ->
    let res = T.Top_result.filter ~provers ~dir res in
    let b = T.Bench.make res in
    Format.printf "%a@." T.Bench.pp b;
    E.return ()
end

(** {2 Sample} *)
module Sample = struct
  open E.Infix

  let run ~n dirs =
    Lwt_list.map_p
      (fun d -> Problem_run.of_dir ~filter:(fun _ -> true) d)
      dirs |> E.ok
    >|= List.flatten
    >|= Array.of_list
    >>= fun files ->
    let len = Array.length files in
    begin
      if len < n
      then E.failf "not enough files (need %d, got %d)" n len
      else E.return ()
    end
    >>= fun () ->
    (* sample the list *)
    let sample_idx =
      CCRandom.sample_without_replacement
        ~compare:CCInt.compare n (CCRandom.int len)
      |> CCRandom.run ?st:None
    in
    let sample = List.map (Array.get files) sample_idx in
    (* print sample *)
    List.iter (Printf.printf "%s\n%!") sample;
    Lwt.return (Ok ())
end

(** {2 List} *)
module List_run = struct
  let pp_snap_summary out (s:Event.Meta.t): unit =
    let provers = Event.Meta.provers s |> Prover.Set.elements in
    let len = Event.Meta.length s in
    Format.fprintf out "@[<h>uuid: %s time: %a num: %d provers: [@[<h>%a@]]@]"
      (Uuidm.to_string (Event.Meta.uuid s))
      ISO8601.Permissive.pp_datetime (Event.Meta.timestamp s) len
      (Misc.Fmt.pp_list ~start:"" ~stop:"" ~sep:"," Prover.pp_name) provers

  let main () =
    let open E in
    let storage = Storage.make [] in
    Event_storage.list_meta storage >>= fun l ->
    (* sort: most recent first *)
    let l =
      List.sort (fun s1 s2 -> compare s2.Event.s_timestamp s1.Event.s_timestamp) l
    in
    Format.printf "@[<v>%a@]@."
      (Misc.Fmt.pp_list ~start:"" ~stop:"" ~sep:"" pp_snap_summary) l;
    E.return ()
end

(** {2 Global Summary}

    Summary of a snapshot compared to other ones with similar provers and
    files *)
module Summary_run = struct
  let main (name:string option) : _ E.t =
    let open E in
    let storage = Storage.make [] in
    Test_run.find_or_last ~storage name >>= fun main_res ->
    Test_run.all_results storage >>= fun l ->
    let summary = Test.Summary.make main_res l in
    Format.printf "@[<v>%a@]@." Test.Summary.print summary;
    E.return ()
end

(** {2 Deletion of snapshots} *)
module Delete_run = struct
  let main (names:string list) : unit E.t =
    let open E in
    let storage = Storage.make [] in
    E.map_s (fun file -> Storage.delete storage file) names >|= fun _ -> ()
end

module Plot_run = struct
  (* Plot functions *)
  let main ~config params (name:string option) : unit E.t =
    let open E in
    let storage = Storage.make [] in
    Test_run.find_or_last ~storage name >>= fun main_res ->
    Test_run.Plot_res.draw_file params main_res;
    E.return ()
end

(** {2 Main: Parse CLI} *)

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

(* sub-command for running tests *)
let term_run =
  let open Cmdliner in
  let aux dyn port dirs dir_file config j timeout memory with_lock nocaching meta save provers junit =
    let caching = not nocaching in
    Lwt_main.run
      (Run.main ~dyn ~port ?j ?timeout ?memory ?junit ?provers ~with_lock
         ~caching ~meta ~save ~config ?dir_file dirs ())
  in
  let config = config_term
  and dyn =
    Arg.(value & flag & info ["progress"] ~doc:"print progress bar")
  and j =
    Arg.(value & opt (some int) None & info ["j"] ~doc:"parallelism level")
  and dir_file =
    Arg.(value & opt (some string) None & info ["F"] ~doc:"file containing a list of files")
  and timeout =
    Arg.(value & opt (some int) None & info ["t"; "timeout"] ~doc:"timeout (in s)")
  and memory =
    Arg.(value & opt (some int) None & info ["m"; "memory"] ~doc:"memory (in MB)")
  and meta =
    Arg.(value & opt string "" & info ["meta"] ~doc:"additional metadata to save")
  and with_loc =
    Arg.(value & opt bool true & info ["lock"] ~doc:"require a lock")
  and nocaching =
    Arg.(value & flag & info ["no-caching"] ~doc:"toggle caching")
  and doc =
    "test a program on every file in a directory"
  and junit =
    Arg.(value & opt (some string) None & info ["junit"] ~doc:"junit output file")
  and save =
    Arg.(value & opt string "" & info ["save"] ~doc:"JSON file to save results in")
  and dir =
    Arg.(value & pos_all string [] &
         info [] ~docv:"DIR" ~doc:"target directories (containing tests)")
  and port =
    let doc = "Local port for the lock daemon" in
    Arg.(value & opt int IPC_daemon.default_port & info ["port"] ~docv:"PORT" ~doc)
  and provers =
    Arg.(value & opt (some (list string)) None & info ["p"; "provers"] ~doc:"select provers")
  in
  Term.(pure aux $ dyn $ port $ dir $ dir_file $ config $ j $ timeout $ memory $ with_loc
    $ nocaching $ meta $ save $ provers $ junit),
  Term.info ~doc "run"

let snapshot_name_term : string option Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(value & pos 0 (some string) None
       & info [] ~docv:"FILE" ~doc:"file/name containing results (default: last)")

(* sub-command to display a file *)
let term_display =
  let open Cmdliner in
  let aux name provers dir = Lwt_main.run (Display.main name provers dir) in
  let name_ =
    Arg.(value & pos 0 (some string) None & info []
           ~docv:"FILE" ~doc:"file containing results (default: last)")
  and dir =
    Arg.(value & pos_right 0 string [] &
         info [] ~docv:"DIR" ~doc:"target directories (containing tests)")
  and provers =
    Arg.(value & opt (some (list string)) None & info ["p"; "provers"] ~doc:"select provers")
  and doc = "display test results from a file" in
  Term.(pure aux $ name_ $ provers $ dir), Term.info ~doc "display"

(* sub-command to display a file as a benchmark *)
let term_bench =
  let open Cmdliner in
  let aux name provers dir = Lwt_main.run (Display_bench.main name provers dir) in
  let provers =
    Arg.(value & opt (some (list string)) None & info ["p"; "provers"] ~doc:"select provers")
  and dir =
    Arg.(value & pos_all string [] &
         info [] ~docv:"DIR" ~doc:"target directories (containing tests)")
  and name_ =
    Arg.(value & pos 0 (some string) None & info [] ~docv:"FILE"
           ~doc:"file containing results (default: last)")
  and doc = "display test results from a file" in
  Term.(pure aux $ name_ $ provers $ dir), Term.info ~doc "bench"

(* sub-command to sample a directory *)
let term_sample =
  let open Cmdliner in
  let aux n dir = Lwt_main.run (Sample.run ~n dir) in
  let dir =
    Arg.(value & pos_all string [] &
         info [] ~docv:"DIR" ~doc:"target directories (containing tests)")
  and n =
    Arg.(value & opt int 1 & info ["n"] ~docv:"N" ~doc:"number of files to sample")
  and doc = "sample N files in the directories" in
  Term.(pure aux $ n $ dir), Term.info ~doc "sample"

(* sub-command to display a file *)
let term_csv =
  let open Cmdliner in
  let aux file out = Lwt_main.run (CSV.main file out) in
  let file =
    Arg.(value & pos 0 (some string) None & info [] ~docv:"FILE" ~doc:"file containing results (default: last)")
  and out =
    Arg.(value & opt (some string) None & info ["o"; "output"]
           ~docv:"OUT" ~doc:"file into which to print (default: stdout)")
  and doc = "dump results as CSV" in
  (* TODO: out should be "-o" option *)
  Term.(pure aux $ file $ out), Term.info ~doc "csv"

(* sub-command to compare two files *)
let term_compare =
  let open Cmdliner in
  let aux file1 file2 = Lwt_main.run (Compare.main ~file1 ~file2 ()) in
  let file1 = Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE1" ~doc:"first file")
  and file2 = Arg.(required & pos 1 (some string) None & info [] ~docv:"FILE2" ~doc:"second file")
  and doc = "compare two result files" in
  Term.(pure aux $ file1 $ file2), Term.info ~doc "compare"

let term_list =
  let open Cmdliner in
  let aux () = Lwt_main.run (List_run.main ()) in
  let doc = "compare two result files" in
  Term.(pure aux $ pure ()), Term.info ~doc "list snapshots"

let drawer_term =
  let open Cmdliner in
  let open Test_run.Plot_res in
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
  let open Test_run.Plot_res in
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

(* sub-command to compare a snapshot to the others *)
let term_summary =
  let open Cmdliner in
  let aux name = Lwt_main.run (Summary_run.main name) in
  let doc = "summary of results from a file, compared to the other snapshots" in
  Term.(pure aux $ snapshot_name_term), Term.info ~doc "summary"

let term_delete =
  let open Cmdliner in
  let aux name = Lwt_main.run (Delete_run.main name) in
  let file_name =
    Arg.(value & pos_all string []
         & info [] ~docv:"FILE" ~doc:"files/names containing results")
  and doc = "delete some snapshots" in
  Term.(pure aux $ file_name), Term.info ~doc "delete result(s)"

let parse_opt () =
  let open Cmdliner in
  let help =
    let doc = "Offers various utilities to test automated theorem provers." in
    let man = [
      `S "DESCRIPTION";
      `P "$(b,frogtest) is a set of utils to run tests, save their results,
          and compare different results obtained with distinct versions of
          the same tool";
      `S "COMMANDS";
      `S "OPTIONS"; (* TODO: explain config file *)
    ] in
    Term.(ret (pure (fun () -> `Help (`Pager, None)) $ pure ())),
    Term.info ~version:"dev" ~man ~doc "frogtest"
  in
  Cmdliner.Term.eval_choice
    help [ term_run; term_compare; term_display; term_csv; term_list;
           term_summary; term_plot; term_bench; term_delete;
           term_sample; ]

let () =
  match parse_opt () with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok (Ok ()) -> ()
  | `Ok (Error e) ->
      print_endline ("error: " ^ e);
      exit 1
