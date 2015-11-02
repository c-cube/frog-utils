
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

type 'a or_error = [`Ok of 'a | `Error of string]
type 'a printer = Format.formatter -> 'a -> unit

module MStr = Map.Make(String)
module Prover = FrogProver

let fpf = Format.fprintf
let spf = Format.asprintf

(** {2 Result on a single problem} *)
module Res = struct
  type t =
    | Sat
    | Unsat
    | Unknown
    | Error

  let print out s =
    Format.pp_print_string out (match s with
      | Sat -> "sat"
      | Unsat -> "unsat"
      | Unknown -> "unknown"
      | Error -> "error"
    )
end

module Problem = struct
  type t = {
    name: string;  (* filename *)
    expected: Res.t; (* result expected *)
  }

  (* regex + mark *)
  let m_unsat_, unsat_ = Re.(str "unsat" |> no_case |> mark)
  let m_sat_, sat_ = Re.(str "sat" |> no_case |> mark)
  let m_unknown_, unknown_ = Re.(str "unknown" |> no_case |> mark)
  let m_error_, error_ = Re.(alt [str "error"; str "fail"] |> no_case |> mark)

  let re_unsat_ = Re.compile unsat_
  let re_sat_ = Re.compile sat_
  let re_unknown_ = Re.compile unknown_
  let re_error_ = Re.compile error_

  (* infer expected status from file name *)
  let find_expected_str_ s =
    if Re.execp re_unsat_ s then Some Res.Unsat
    else if Re.execp re_sat_ s then Some Res.Sat
    else if Re.execp re_unknown_ s then Some Res.Unknown
    else if Re.execp re_error_ s then Some Res.Error
    else None

  (* "^ #expect: (unsat|sat|unknown|error)", basically *)
  let re_expect_ = Re.(seq
    [ str "expect:" |> no_case ; rep blank;
      alt [unsat_; sat_; unknown_; error_] ]
    |> compile
  )

  (* what is expected? *)
  let find_expected_ ~file =
    match find_expected_str_ file with
    | Some r -> Lwt.return r
    | None ->
        let%lwt content = FrogMisc.File.with_in ~file FrogMisc.File.read_all in
        let g = Re.exec re_expect_ content in
        if Re.marked g m_unsat_ then Lwt.return Res.Unsat
          else if Re.marked g m_sat_ then Lwt.return Res.Sat
          else if Re.marked g m_unknown_ then Lwt.return Res.Unknown
          else if Re.marked g m_error_ then Lwt.return Res.Error
          else Lwt.fail Not_found

  let make ~file =
    try%lwt
      FrogDebug.debug "convert %s into problem..." file;
      let%lwt res = find_expected_ ~file in
      let pb = {
        name=file;
        expected=res;
      } in
      Lwt.return (`Ok pb)
    with e ->
      Lwt.return
        (`Error (spf "could not find expected res for %s: %s"
                file (Printexc.to_string e)))

  let compare_res pb res =
    match pb.expected, res with
    | Res.Unsat, Res.Unsat
    | Res.Sat, Res.Sat
    | Res.Unknown, Res.Unknown
    | Res.Error, Res.Error -> `Same
    | (Res.Unsat | Res.Error), Res.Sat
    | (Res.Sat | Res.Error), Res.Unsat
    | (Res.Sat | Res.Unsat | Res.Error), Res.Unknown
    | (Res.Sat | Res.Unknown | Res.Unsat), Res.Error ->
        `Mismatch
    | Res.Unknown, (Res.Sat | Res.Unsat) ->
        `Improvement

  let print out p =
    fpf out "@[<h>%s (expect: %a)@]" p.name Res.print p.expected
end

module ProblemSet = struct
  type t = Problem.t list

  let make l =
    let pool = Lwt_pool.create 30 (fun () -> Lwt.return_unit) in
    let%lwt l =
      Lwt_list.map_p
        (fun file ->
          Lwt_pool.use pool
            (fun () -> Problem.make ~file)
        ) l
    in
    Lwt.return (FrogMisc.Err.seq_list l)

  let size = List.length

  let of_dir ?filter:(p=fun _ -> true) d =
    let l = FrogMisc.File.walk d in
    let l = FrogMisc.List.filter_map
      (fun (kind,f) -> match kind with
        | `File when p f -> Some f
        | _ -> None
      ) l
    in
    make l

  let print out set =
    fpf out "@[<hv>%a@]" (Format.pp_print_list Problem.print) set
end

module Config = struct
  type t = {
    j: int; (* number of concurrent processes *)
    timeout: int; (* timeout for each problem *)
    problem_pat: Re.re; (* regex for problems *)
    prover: Prover.t;
  }

  let make ?(j=1) ?(timeout=5) ~pat ~prover () =
    { j; timeout; prover; problem_pat=pat; }

  let of_file file =
    let module E = FrogMisc.Err in
    let module C = FrogConfig in
    FrogDebug.debug "parse config file %s..." file;
    try
      let c = C.parse_files [file] C.empty in
      let j = C.get_int ~default:1 c "parallelism" in
      let timeout = C.get_int ~default:5 c "timeout" in
      let prover = C.get_string c "prover" in
      let prover = Prover.build_from_config c prover in
      let problem_pat = C.get_string c "problems" in
      let problem_pat = Re_posix.compile_pat problem_pat in
      E.return { j; timeout; prover; problem_pat; }
    with
    | C.Error e -> E.fail e
    | Not_found -> E.fail ("invalid config file: " ^ file)
end

module Results = struct
  type raw = (Problem.t * Res.t) MStr.t

  type stat = {
    unsat: int;
    sat: int;
    errors: int;
    unknown: int;
  }

  let stat_empty = {unsat=0; sat=0; errors=0; unknown=0; }

  let add_sat_ s = {s with sat=s.sat+1}
  let add_unsat_ s = {s with unsat=s.unsat+1}
  let add_unknown_ s = {s with unknown=s.unknown+1}
  let add_error_ s = {s with errors=s.errors+1}

  let pp_stat out s =
    fpf out "@[<h>unsat: %d, sat: %d, errors: %d, unknown: %d (%d total)@]"
      s.unsat s.sat s.errors s.unknown (s.unsat + s.sat + s.errors + s.unknown)

  type t = {
    raw: raw;
    stat: stat;
    improved: (Problem.t * Res.t) list;
    mismatch: (Problem.t * Res.t) list;
  }

  let pp_pb_res_ out (pb, res) =
    fpf out "@[<h>problem %s (expected: %a, result: %a)@]"
      pb.Problem.name Res.print pb.Problem.expected Res.print res

  let pp_list_ p = Format.pp_print_list p

  (* build statistics and list of mismatch from raw results *)
  let analyse_ raw =
    let improved = ref [] in
    let mismatch = ref [] in
    let stat = ref stat_empty in
    let add_res res =
      stat := (match res with
        | Res.Unsat -> add_unsat_ | Res.Sat -> add_sat_
        | Res.Unknown -> add_unknown_ | Res.Error -> add_error_
      ) !stat
    in
    MStr.iter
      (fun _ (pb, res) ->
        add_res res;
        match Problem.compare_res pb res with
        | `Same -> ()
        | `Mismatch ->
            mismatch := (pb,res) :: !mismatch;
        | `Improvement ->
            improved := (pb,res) :: !improved
      )
      raw;
    !improved, !mismatch, !stat

  let make raw =
    let improved, mismatch, stat = analyse_ raw in
    { raw; stat; improved; mismatch; }

  let of_list l =
    let raw = List.fold_left
      (fun acc (pb,res) -> MStr.add pb.Problem.name (pb,res) acc)
      MStr.empty l
    in
    make raw

  let is_ok r = r.mismatch = []

  let print out r =
    fpf out
      "@[<hv2>results: {@,stat:%a,@ improved: @[<hv>%a@],@ mismatch: @[<hv>%a@]@,@]}"
      pp_stat r.stat
      (pp_list_ pp_pb_res_) r.improved
      (pp_list_ pp_pb_res_) r.mismatch
end

let extract_res_ ~prover stdout errcode =
  (* find if [re: re option] is present in [stdout] *)
  let find_opt_ re = match re with
    | None -> false
    | Some re -> Re.execp (Re_posix.compile_pat re) stdout
  in
  if errcode <> 0 then Res.Error
  else if find_opt_ prover.Prover.sat then Res.Sat
  else if find_opt_ prover.Prover.unsat then Res.Unsat
  else if (find_opt_ prover.Prover.timeout || find_opt_ prover.Prover.unknown)
    then Res.Unknown
    else Res.Error

(* run one particular test *)
let run_pb ~config pb =
  FrogDebug.debug "running %-30s..." pb.Problem.name;
  (* spawn process *)
  let%lwt (out,_err,errcode) = Prover.run_proc
    ~timeout:config.Config.timeout
    ~prover:config.Config.prover
    ~file:pb.Problem.name
    ()
  in
  (* parse its output *)
  let actual = extract_res_ ~prover:config.Config.prover out errcode in
  Lwt.return actual

let nop2_ _ _ = Lwt.return_unit

let run ?(on_solve = nop2_) ~config set =
  let pool = Lwt_pool.create config.Config.j (fun () -> Lwt.return_unit) in
  let%lwt raw =
    Lwt_list.map_p
      (fun pb ->
        Lwt_pool.use pool
          (fun () ->
            let%lwt res = run_pb ~config pb in
            let%lwt () = on_solve pb res in
            Lwt.return (pb, res)
          )
      )
      set
  in
  Lwt.return (Results.of_list raw)

