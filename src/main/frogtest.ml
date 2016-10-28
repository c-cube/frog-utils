
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(* run tests, or compare results *)

open Result
open Frog
open Frog_server
module T = Test
module E = Misc.LwtErr
module W = Web

(** {2 Run} *)
module Run = struct
  (* callback that prints a result *)
  let on_solve res =
    let module F = Misc.Fmt in
    let p_res = Event.analyze_p res in
    let pp_res out () =
      let str, c = match Problem.compare_res res.Event.problem p_res with
        | `Same -> "ok", `Green
        | `Improvement -> "ok (improved)", `Blue
        | `Disappoint -> "disappoint", `Yellow
        | `Mismatch -> "bad", `Red
      in
      Format.fprintf out "%a" (F.in_bold_color c Format.pp_print_string) str
    in
    let prover = res.Event.program in
    let prover_name = Filename.basename prover.Prover.name in
    let pb_name = res.Event.problem.Problem.name in
    Lwt_log.ign_debug_f "result for `%s` with %s: %s"
       prover_name pb_name (Res.to_string p_res);
    Format.printf "%-20s%-50s %a@." prover_name (pb_name ^ " :") pp_res ();
    Lwt.return_unit

  (* run provers on the given dir, return a list [prover, dir, results] *)
  let test_dir ?j ?timeout ?memory ?caching ?provers ~config ~problem_pat dir
    : T.Top_result.t E.t =
    let open E in
    Format.printf "testing dir `%s`...@." dir;
    ProblemSet.of_dir
      ~default_expect:config.T.Config.default_expect
      ~filter:(Re.execp problem_pat)
      dir
    >>= fun pbs ->
    Format.printf "run %d tests in %s@." (ProblemSet.size pbs) dir;
    (* solve *)
    let main =
      E.ok (Test_run.run ?j ?timeout ?memory ?caching ?provers
          ~on_solve ~config pbs)
    in
    main
    >>= fun results ->
    Prover.Map.iter
      (fun p r ->
         Format.printf "@[<2>%s on `%s`:@ @[<hv>%a@]@]@."
           (Prover.name p) dir T.Analyze.print r)
      (Lazy.force results.T.analyze);
    E.return results

  let check_res (results:T.top_result) : unit E.t =
    let lazy map = results.T.analyze in
    if Prover.Map.for_all (fun _ r -> T.Analyze.is_ok r) map
    then E.return ()
    else
      E.fail (Format.asprintf "%d failure(s)" (
          Prover.Map.fold
            (fun _ r n -> n + T.Analyze.num_failed r)
            map 0))

  (* lwt main *)
  let main ?j ?timeout ?memory ?caching ?junit ?provers ?meta ~save ~config dirs () =
    let open E in
    (* parse config *)
    Lwt.return (Test_run.config_of_file config)
    >>= fun config ->
    (* pick default directory if needed *)
    let dirs = match dirs with
      | _::_ -> dirs
      | [] -> config.T.Config.default_dirs
    in
    let storage = Storage.make [] in
    (* build problem set (exclude config file!) *)
    let problem_pat = Re_posix.compile_pat config.T.Config.problem_pat in
    E.map_s
      (test_dir ?j ?timeout ?memory ?caching ?provers ~config ~problem_pat)
      dirs
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
          |> Prover.Map.to_list
          |> List.map (fun (_,a) -> JUnit_wrapper.test_analyze a) in
        JUnit_wrapper.junit_to_file suites file;
    end;
    (* now fail if results were bad *)
    check_res results
end

(** {2 Display Run} *)
module Display = struct
  let main ~file () =
    let open E in
    let storage = Storage.make [] in
    Test_run.find_results ~storage file >>= fun res ->
    Format.printf "%a@." T.Top_result.pp res;
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

(** {2 List} *)
module List_run = struct
  let pp_snap_summary out (s:Event.Snapshot.t): unit =
    let provers = Event.Snapshot.provers s |> Prover.Set.elements in
    let len = List.length s.Event.events in
    Format.fprintf out "@[<h>uuid: %s time: %a num: %d provers: [@[<h>%a@]]@]"
      (Uuidm.to_string s.Event.uuid)
      ISO8601.Permissive.pp_datetime s.Event.timestamp len
      (Misc.Fmt.pp_list ~start:"" ~stop:"" ~sep:"," Prover.pp_name) provers

  let main () =
    let open E in
    let storage = Storage.make [] in
    Event_storage.list_snapshots storage >>= fun l ->
    Format.printf "@[<v>%a@]@."
      (Misc.Fmt.pp_list ~start:"" ~stop:"" ~sep:"" pp_snap_summary) l;
    E.return ()
end

(** {2 Global Summary}

    Summary of a snapshot compared to other ones with similar provers and
    files *)
module Summary_run = struct

end

(** {2 Main: Parse CLI} *)

(* sub-command for running tests *)
let term_run =
  let open Cmdliner in
  let aux dirs debug config j timeout memory nocaching meta save provers junit =
    let config = Config.interpolate_home config in
    if debug then (
      Maki_log.set_level 5;
      Lwt_log.add_rule "*" Lwt_log.Debug;
    );
    let caching = not nocaching in
    Lwt_main.run
      (Run.main ?j ?timeout ?memory ?junit ?provers ~caching ~meta ~save ~config dirs ())
  in
  let debug =
    Arg.(value & flag & info ["d"; "debug"] ~doc:"enable debug")
  and config =
    Arg.(value & opt string "$home/.frogutils.toml" &
    info ["c"; "config"] ~doc:"configuration file (in target directory)")
  and j =
    Arg.(value & opt (some int) None & info ["j"] ~doc:"parallelism level")
  and timeout =
    Arg.(value & opt (some int) None & info ["t"; "timeout"] ~doc:"timeout (in s)")
  and memory =
    Arg.(value & opt (some int) None & info ["m"; "memory"] ~doc:"memory (in MB)")
  and meta =
    Arg.(value & opt string "" & info ["meta"] ~doc:"additional metadata to save")
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
  and provers =
    Arg.(value & opt (some (list string)) None & info ["p"; "provers"] ~doc:"select provers")
  in
  Term.(pure aux $ dir $ debug $ config $ j $ timeout $ memory
    $ nocaching $ meta $ save $ provers $ junit),
  Term.info ~doc "run"

(* sub-command to display a file *)
let term_display =
  let open Cmdliner in
  let aux file = Lwt_main.run (Display.main ~file ()) in
  let file =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc:"file containing results")
  and doc = "display test results from a file" in
  Term.(pure aux $ file), Term.info ~doc "display"

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
    help [ term_run; term_compare; term_display; term_list ]

let () =
  match parse_opt () with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok (Ok ()) -> ()
  | `Ok (Error e) ->
      print_endline ("error: " ^ e);
      exit 1
