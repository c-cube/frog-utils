
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Tests} *)

open Result
open Frog
open Lwt.Infix

type 'a or_error = 'a Misc.Err.t
type path = string

module C = Test.Config
module T = Test
module E = Misc.LwtErr

let expect_of_config config = function
  | None -> Ok C.Auto
  | Some s ->
    let open Misc.Err in
    begin match Misc.Str.split ~by:':' s with
      | "program", p ->
        ProverSet.find_config config p >|= fun p -> C.Program p
      | _ -> Ok (C.Res (Res.of_string s))
      | exception Not_found -> Ok (C.Res (Res.of_string s))
    end

let config_of_config ?(profile="test") config dirs =
  let getter =
    let open Config in
    let tbl = table profile in
    (try_tables [tbl; top] @@ int "parallelism" <|> pure 1) >>= fun j ->
    (try_tables [tbl; top] @@ int "timeout" <|> pure 5) >>= fun timeout ->
    (try_tables [tbl; top] @@ int "memory" <|> pure 1000) >>= fun memory ->
    let problem_pat =
      try_tables [tbl; top] @@ string "problems"
    in
    let default_expect =
      (try_tables [tbl; top] @@ string "default_expect" >|= fun x-> Some x)
      <|> pure None
    in
    begin match dirs with
      | [] -> try_tables [tbl; top] @@ string_list ~default:[] "dir"
      | _ -> pure dirs
    end >>= fun l ->
    map_l
      (fun dir_name ->
         let dir_tbl = tbl |>> table dir_name in
         begin
         (dir_tbl |>> string "directory" <|> pure dir_name) >>= fun dir ->
         (dir_tbl |>> string "problems" <|> problem_pat) >>= fun pat ->
         ( (( some @@ try_tables [dir_tbl; tbl; top] @@ string "expect")
            <|> default_expect)
           >>= fun e -> (expect_of_config config e |> pure_or_error) )
         >|= fun expect ->
         { C.directory = dir; pattern = pat; expect = expect; }
         end |> add_ctxf "read config for directory `%s`" dir_name)
      l
    >>= fun problems ->
    ((try_tables [tbl; top] @@ string_list "provers") |> add_ctxf "get provers")
     >>= fun provers ->
    map_l
      (fun p -> ProverSet.find_config config p |> pure_or_error)
      provers
    >>= fun provers ->
    return { C.j; timeout; memory; provers; problems; }
  in
  Config.get config getter

let config_of_file ?profile file =
  Lwt_log.ign_debug_f "parse config file `%s`..." file;
  let open Misc.Err in
  Config.parse_file file >>= fun c ->
  config_of_config ?profile c []

(* run one particular test *)
let run_pb_ ~config prover pb =
  Lwt_log.ign_debug_f "running %-15s/%-30s..."
    (Filename.basename prover.Prover.binary) pb.Problem.name;
  (* spawn process *)
  let%lwt result = Run.run_prover
      ~timeout:config.C.timeout
      ~memory:config.C.memory
      ~prover ~pb ()
  in
  Lwt_log.ign_debug_f "output for %s/%s: `%s`, `%s`, errcode %d"
    prover.Prover.binary pb.Problem.name
    result.Event.raw.Event.stdout
    result.Event.raw.Event.stderr
    result.Event.raw.Event.errcode;
  Lwt.return result

let run_pb ?(caching=true) ?limit ~config prover pb : _ E.t =
  let module V = Maki.Value in
  Maki.call
    ?limit
    ~bypass:(not caching)
    ~lifetime:(`KeepFor Maki.Time.(days 2))
    ~deps:[V.pack V.int config.C.timeout;
           V.pack V.int config.C.memory;
           V.pack Maki_wrapper.prover prover;
           V.pack Maki_wrapper.problem pb]
    ~op:Run.maki_result
    ~name:"frogtest.run_pb"
    (fun () -> run_pb_ ~config prover pb)
  |> E.of_exn

let nop_ _ = Lwt.return_unit

let print_result (res:Test.result): unit =
  let module F = Misc.Fmt in
  let p_res = Event.analyze_p res in
  let pp_res out () =
    let str, c = match Problem.compare_res res.Event.problem p_res with
      | `Same -> "ok", `Green
      | `Improvement -> "ok (improved)", `Blue
      | `Disappoint -> "disappoint", `Cyan
      | `Error -> "error", `Yellow
      | `Mismatch -> "bad", `Red
    in
    Format.fprintf out "%a" (F.in_bold_color c Format.pp_print_string) str
  in
  let prover = res.Event.program in
  let prover_name = Filename.basename prover.Prover.name in
  let pb_name = res.Event.problem.Problem.name in
  Lwt_log.ign_debug_f "result for `%s` with %s: %s (%.1fs)"
    prover_name pb_name (Res.to_string p_res) res.Event.raw.Event.rtime;
  Format.printf "%-20s%-50s %a (%.1fs)@." prover_name (pb_name ^ " :")
    pp_res () res.Event.raw.Event.rtime;
  ()

let run ?(on_solve = nop_) ?(on_done = nop_)
    ?(caching=true) ?j ?timeout ?memory ~provers ~expect ~config (set:path list)
    : Test.top_result E.t =
  let open E.Infix in
  let config = C.update ?j ?timeout ?memory config in
  let j = CCOpt.get_or j ~default:config.C.j in
  let limit = Maki.Limit.create j in
  E.map_p
    (fun pb_path ->
       (* transform into problem *)
       let%lwt pb =
         Maki.Limit.acquire limit
           (fun () ->
              let find_expect = Problem_run.find_expect ~expect in
              Problem_run.make ~find_expect pb_path)
         |> Misc.LwtErr.to_exn
       in
       (* run provers *)
       E.map_p
         (fun prover ->
            run_pb ~caching ~limit ~config prover pb >>= fun result ->
            let%lwt () = on_solve result in (* callback *)
            E.return result
            |> E.add_ctxf "running `%a` on %a"
              Prover.pp_name prover Problem.print pb)
         provers)
    set
  >>= fun res ->
  let res = List.flatten res in
  let r = T.Top_result.make (List.map Event.mk_prover res) in
  let%lwt () = on_done r in
  E.return r

let find_results ?storage str =
  match storage with
    | None -> T.Top_result.of_file str
    | Some storage ->
      let open E in
      let%lwt res1 =
        Event_storage.find_snapshot storage str
        >|= T.Top_result.of_snapshot
      in
      match res1 with
        | Ok x -> E.return x
        | Error _ ->
          T.Top_result.of_file str

let all_results storage =
  let open E in
  Event_storage.list_snapshots storage >>= fun l ->
  E.map_s (fun snap -> T.Top_result.of_snapshot snap |> E.return) l

let last_result storage =
  let open E in
  all_results storage >>= function
  | [] -> E.fail "last_result failed: no result found in storage"
  | x :: l ->
    let best =
      List.fold_left
        (fun best t -> if best.T.timestamp < t.T.timestamp then t else best)
        x l
    in
    E.return best

let find_or_last ?storage str_opt = match str_opt, storage with
  | Some f, _ -> find_results ?storage f
  | None, Some storage -> last_result storage
  | None, None -> E.fail "cannot find last result"
