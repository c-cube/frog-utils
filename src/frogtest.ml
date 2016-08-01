
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(* run tests, or compare results *)

open Result
module T = FrogTest
module Prover = FrogProver
module E = FrogMisc.LwtErr
module W = FrogWeb

(** {2 Results for multiple provers on multiple dirs} *)
module Global_res = struct
  type t = (Prover.t * string * T.Analyze.t) list

  let to_file ~file t = 
    let json =
      `List
        (List.map
           (fun (p,dir,r) ->
              `List [Prover.to_yojson p; `String dir; T.Analyze.to_yojson r])
           t)
    in
    Yojson.Safe.to_file file json

  let of_file ~file : t E.t =
    let open E in
    try
      let j = Yojson.Safe.from_file file in
      begin match j with
        | `List l ->
          E.map_s
            (function
              | `List [p; `String dir; res] ->
                Prover.of_yojson p |> E.lift >>= fun p ->
                T.Analyze.of_yojson res |> E.lift >|= fun res ->
                p, dir, res
              | j ->
                let msg =
                  Printf.sprintf
                    "invalid json for frogtest (expected triple,\nnot %s)"
                    (Yojson.Safe.pretty_to_string j)
                in
                E.fail msg)
            l
        | _ -> E.fail "invalid json for frogtest (expected toplevel list)"
      end
    with e ->
      E.fail (Printexc.to_string e)

  let pp out (r:t) =
    let pp_tup out (p,dir,res) =
      Format.fprintf out "@[<2>%s on `%s`:@ @[%a@]@]"
        (Prover.name p) dir T.Analyze.print res
    in
    Format.fprintf out "@[<v>%a@]" (FrogMisc.Fmt.pp_list pp_tup) r

  type comparison_result = {
    both: (Prover.t * string * T.ResultsComparison.t) list;
    left: (Prover.t * string * T.Analyze.t) list;
    right: (Prover.t * string * T.Analyze.t) list;
  }

  let compare (a:t) (b:t): comparison_result =
    let both, left =
      List.fold_left
        (fun (both,left) (p,dir,r_left) ->
           try
             (* find same (problem,dir) in [b], and compare *)
             let _, _, r_right =
               List.find
                 (fun (p', dir', _) -> Prover.equal p p' && dir = dir')
                 b
             in
             let cmp =
               T.ResultsComparison.compare r_left.T.Analyze.raw  r_right.T.Analyze.raw
             in
             (p, dir, cmp) :: both, left
           with Not_found ->
             both, (p,dir,r_left)::left)
        ([],[]) a
    in
    let right =
      List.filter
        (fun (p,dir,_) ->
           List.for_all
             (fun (p',dir',_) -> not (Prover.equal p p') || dir <> dir')
             a)
        b
    in
    { both; left; right; }

  let pp_comparison out (r:comparison_result) =
    let pp_tup out (p,dir,cmp) =
      Format.fprintf out "@[<2>%s on `%s`:@ @[%a@]@]"
        (Prover.name p) dir T.ResultsComparison.print cmp
    and pp_one which out (p,dir,res) =
      Format.fprintf out "@[<2>%s on `%s` (only on %s):@ @[%a@]@]"
        (Prover.name p) dir which T.Analyze.print res
    in
    Format.fprintf out "@[<hv>%a@,%a@,%a@]@."
      (FrogMisc.Fmt.pp_list pp_tup) r.both
      (FrogMisc.Fmt.pp_list (pp_one "left")) r.left
      (FrogMisc.Fmt.pp_list (pp_one "right")) r.right
end

(** {2 Run} *)
module Run = struct
  (* callback that prints a result *)
  let on_solve res =
    let module F = FrogMisc.Fmt in
    let pp_res out () =
      let str, c =
        match T.Problem.compare_res res.FrogRun.problem (FrogRun.analyze_p res) with
        | `Same -> "ok", `Green
        | `Improvement -> "ok (improved)", `Blue
        | `Disappoint -> "disappoint", `Yellow
        | `Mismatch -> "bad", `Red
      in
      Format.fprintf out "%a" (F.in_bold_color c Format.pp_print_string) str
    in
    let `Prover prover = res.FrogRun.program in
    Format.printf "%-20s%-50s %a@."
      (Filename.basename prover.FrogProver.name)
      (res.FrogRun.problem.FrogProblem.name ^ " :") pp_res ();
    Lwt.return_unit

  (* run provers on the given dir, return a list [prover, dir, results] *)
  let test_dir ?j ?timeout ?memory ?caching ~config ~problem_pat ~web ~db dir
    : (Prover.t * string * T.Analyze.t) list E.t =
    let open E in
    Format.printf "testing dir `%s`...@." dir;
    T.ProblemSet.of_dir ~filter:(Re.execp problem_pat) dir
    >>= fun pbs ->
    Format.printf "run %d tests in %s@." (T.ProblemSet.size pbs) dir;
    (* serve website *)
    let db =
      FrogDB.create
        ~db_path:db
        ~db_init:[
          FrogProver.db_init;
          FrogProblem.db_init;
          FrogRun.db_init;
        ] ()
    in
    let server =
      if web
      then Some (W.Server.create ~db ())
      else None
    in
    (* solve *)
    let main =
      E.ok (T.run ?j ?timeout ?memory ?caching ~on_solve ?server ~db ~config pbs)
    in
    let web = FrogMisc.Opt.((server >|= W.Server.run) |> get Lwt.return_unit) |> E.ok in
    main
    >|= List.map (fun (p,r) -> p, dir, r) (* add directory *)
    >>= fun results ->
    List.iter
      (fun (p,_,r) ->
         Format.printf "@[<2>%s on `%s`:@ @[<hv>%a@]@]@."
           (Prover.name p) dir T.Analyze.print r)
      results;
    (* wait for webserver to return *)
    let%lwt _ = web in
    E.return results

  let check_res results : unit E.t =
    if List.for_all (fun (_,_,r) -> T.Analyze.is_ok r) results
    then E.return ()
    else
      E.fail (Format.asprintf "%d failure(s)" (
          List.fold_left
            (fun n (_,_,r) -> n+T.Analyze.num_failed r)
            0 results))

  (* lwt main *)
  let main ?j ?timeout ?memory ?caching ?junit ~web ~save ~db ~config dirs () =
    let open E in
    (* parse config *)
    Lwt.return (T.Config.of_file config)
    >>= fun config ->
    (* pick default directory if needed *)
    let dirs = match dirs with
      | _::_ -> dirs
      | [] -> config.T.Config.default_dirs
    in
    (* build problem set (exclude config file!) *)
    let problem_pat = Re_posix.compile_pat config.T.Config.problem_pat in
    E.map_s
      (test_dir ?j ?timeout ?memory ?caching ~config ~problem_pat ~web ~db)
      dirs
    >|= List.flatten
    >>= fun (results:Global_res.t) ->
    begin match save with
      | None -> ()
      | Some file ->
        Global_res.to_file ~file results
    end;
    begin match junit with
      | None -> ()
      | Some file ->
        Lwt_log.ign_info_f "write results in Junit to file `%s`" file;
        let suites = results |> List.map (fun (_,_,r) -> T.Analyze.to_junit r) in
        T.Analyze.junit_to_file suites file;
    end;
    (* now fail if results were bad *)
    check_res results
end

(** {2 Display FrogRun} *)
module Display = struct
  let main ~file () =
    let open E in
    Global_res.of_file ~file >>= fun res ->
    Format.printf "%a@." Global_res.pp res;
    E.return ()
end

(** {2 Compare FrogRun} *)
module Compare = struct
  let main ~file1 ~file2 () =
    let open E in
    Global_res.of_file ~file:file1 >>= fun res1 ->
    Global_res.of_file ~file:file2 >>= fun res2 ->
    let cmp = Global_res.compare res1 res2 in
    Format.printf "%a@." Global_res.pp_comparison cmp;
    E.return ()
end

(** {2 Main: Parse CLI} *)

(* sub-command for running tests *)
let term_run =
  let open Cmdliner in
  let aux dirs debug config j timeout memory nocaching web save db junit =
    let config = FrogConfig.interpolate_home config in
    if debug then (
      Maki_log.set_level 5;
      Lwt_log.add_rule "*" Lwt_log.Debug;
    );
    let caching = not nocaching in
    Lwt_main.run
      (Run.main ?j ?timeout ?memory ?junit ~caching ~web ~save ~db ~config dirs ())
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
  and web =
    Arg.(value & flag & info ["web"] ~doc:"embedded web server")
  and db =
    Arg.(value & opt string "$HOME/.frogdb.sql" & info ["db"] ~doc:"path to database")
  and nocaching =
    Arg.(value & flag & info ["no-caching"] ~doc:"toggle caching")
  and doc =
    "test a program on every file in a directory"
  and junit =
    Arg.(value & opt (some string) None & info ["junit"] ~doc:"junit output file")
  and save =
    Arg.(value & opt (some string) None & info ["save"] ~doc:"JSON file to save results in")
  and dir =
    Arg.(value & pos_all string [] &
         info [] ~docv:"DIR" ~doc:"target directories (containing tests)")
  in
  Term.(pure aux $ dir $ debug $ config $ j $ timeout $ memory
    $ nocaching $ web $ save $ db $ junit),
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
    help [ term_run; term_compare; term_display ]

let () =
  match parse_opt () with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok (Ok ()) -> ()
  | `Ok (Error e) ->
      print_endline ("error: " ^ e);
      exit 1
