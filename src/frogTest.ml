
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
  [@@@warning "-39"]
  type t =
    | Sat
    | Unsat
    | Unknown
    | Error
    [@@deriving yojson]
  [@@@warning "+39"]

  let print out s =
    Format.pp_print_string out (match s with
      | Sat -> "sat"
      | Unsat -> "unsat"
      | Unknown -> "unknown"
      | Error -> "error"
    )

  let compare a b = match a, b with
    | Unsat, Unsat
    | Sat, Sat
    | Unknown, Unknown
    | Error, Error -> `Same
    | Unknown, (Sat | Unsat) -> `RightBetter
    | (Sat | Unsat), Unknown -> `LeftBetter
    | (Unsat | Error), Sat
    | (Sat | Error), Unsat
    | Error, Unknown
    | (Sat | Unknown | Unsat), Error ->
        `Mismatch

  let maki : t Maki.Value.ops = Maki_yojson.make_err ~to_yojson ~of_yojson "result"
end

module Problem = struct
  [@@@warning "-39"]
  type t = {
    name: string;  (* filename *)
    expected: Res.t; (* result expected *)
  } [@@deriving yojson]
  [@@@warning "+39"]

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
    [ alt (List.map no_case [str "expect:"; str "expected:"]) ; rep blank;
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
      Lwt_log.ign_debug_f "convert `%s` into problem..." file;
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

  let maki =
    let module V = Maki.Value in
    V.map ~descr:"problem"
      (fun p -> p.name, p.expected)
      (fun (name, expected) -> {name;expected})
      (V.pair V.file Res.maki)
end

module ProblemSet = struct
  type t = Problem.t list

  let make l =
    (* sort by alphabetic order *)
    let l = List.sort String.compare l in
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

  let maki = Maki.Value.set Problem.maki
end

module Config = struct
  type t = {
    j: int; (* number of concurrent processes *)
    timeout: int; (* timeout for each problem *)
    memory: int;
    problem_pat: string; (* regex for problems *)
    prover: Prover.t;
  } [@@deriving yojson]

  let maki : t Maki.Value.ops =
    let module V = Maki.Value in
    let json =  Maki_yojson.make_err ~of_yojson ~to_yojson "config_json" in
    V.map ~descr:"config"
      (fun t -> t, t.prover.Prover.binary)
      (fun (t,_) -> t)
      (V.pair json V.program)

  let make ?(j=1) ?(timeout=5) ?(memory=1000) ~pat ~prover () =
    { j; timeout; memory; prover; problem_pat=pat; }

  let update ?j ?timeout ?memory c =
    let module O = FrogMisc.Opt in
    let j = O.get c.j j in
    let timeout = O.get c.timeout timeout in
    let memory = O.get c.memory memory in
    { c with j; timeout; memory; }

  let of_file file =
    let module E = FrogMisc.Err in
    let module C = FrogConfig in
    Lwt_log.ign_debug_f "parse config file `%s`..." file;
    try
      let c = C.parse_files [file] C.empty in
      let j = C.get_int ~default:1 c "parallelism" in
      let timeout = C.get_int ~default:5 c "timeout" in
      let memory = C.get_int ~default:1000 c "memory" in
      let prover = C.get_string c "prover" in
      let prover = Prover.build_from_config c prover in
      let problem_pat = C.get_string c "problems" in
      E.return { j; timeout; memory; prover; problem_pat; }
    with
    | C.Error e -> E.fail e
    | Not_found -> E.fail ("invalid config file: " ^ file)
end

module Results = struct
  type raw = (Problem.t * Res.t) MStr.t

  let raw_of_list l =
    List.fold_left
      (fun acc (pb,res) -> MStr.add pb.Problem.name (pb,res) acc)
      MStr.empty l

  let raw_to_yojson r : Yojson.Safe.json =
    let l = MStr.fold
      (fun _ (pb,res) acc ->
        (`List [Problem.to_yojson pb; Res.to_yojson res] :: acc))
      r []
    in
    `List l

  module E = FrogMisc.Err

  let raw_of_yojson (j:Yojson.Safe.json) : raw or_error =
    let open E in
    let get_list_ = function
      | `List l -> E.return l
      | _ -> E.fail "expected list"
    in
    (* parse a single (problem, res) pair *)
    let get_pair_ j =
      get_list_ j >>= function
      | [a;b] ->
          Problem.of_yojson a >>= fun a ->
          Res.of_yojson b >>= fun b ->
          E.return (a,b)
      | _ -> E.fail "expected pair"
    in
    get_list_ j >|= List.map get_pair_ >>= E.seq_list >|= raw_of_list

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
    fpf out "{@[<hv>unsat: %d,@ sat: %d,@ errors: %d,@ unknown: %d,@ total: %d@]}"
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

  let of_yojson j = E.(raw_of_yojson j >|= make)
  let to_yojson t = raw_to_yojson t.raw

  let of_list l =
    let raw = raw_of_list l in
    make raw

  let of_file ~file =
    try
      let json = Yojson.Safe.from_file file in
      of_yojson json
    with e -> E.fail (Printexc.to_string e)

  let to_file t ~file = Yojson.Safe.to_file file (to_yojson t)

  let is_ok r = r.mismatch = []
  let num_failed r = List.length r.mismatch

  let print out r =
    fpf out
      "@[<hv2>results: {@,stat:%a,@ improved: [@[<hv>%a@]],@ mismatch: [@[<hv>%a@]]@]@,}"
      pp_stat r.stat
      (pp_list_ pp_pb_res_) r.improved
      (pp_list_ pp_pb_res_) r.mismatch

  let maki =
    Maki.Value.make_fast "results"
      ~serialize:(fun p ->
        to_yojson p |> (fun x->Yojson.Safe.to_string x) |> Maki_bencode.mk_str)
      ~unserialize:(function
        | Bencode.String s ->
          begin try
              Yojson.Safe.from_string s |> of_yojson
              |> (function
                |`Ok x -> Result.Ok x
                | `Error y -> Result.Error (Failure y))
            with e -> Result.Error e
          end
        | _ -> Result.Error (Failure "expected string"))
end

module ResultsComparison = struct
  type t = {
    appeared: (Problem.t * Res.t) list;  (* new problems *)
    disappeared: (Problem.t * Res.t) list; (* problems that disappeared *)
    improved: (Problem.t * Res.t * Res.t) list;
    regressed: (Problem.t * Res.t * Res.t) list;
    mismatch: (Problem.t * Res.t * Res.t) list;
    same: (Problem.t * Res.t) list; (* same result *)
  }

  let compare (a:Results.raw) b =
    let appeared = ref []
    and disappeared = ref []
    and improved = ref []
    and regressed = ref []
    and mismatch = ref []
    and same = ref [] in
    (* hack? we want to match each pair of key of a and b *)
    ignore
      (MStr.merge
        (fun _ v1 v2 ->
          begin match v1, v2 with
          | Some (pb,res), None ->
              disappeared := (pb, res) :: !disappeared
          | None, Some (pb,res) ->
              appeared := (pb, res) :: !appeared
          | None, None -> assert false
          | Some (pb1,res1), Some (pb2, res2) ->
              assert (pb1.Problem.name = pb2.Problem.name);
              match Res.compare res1 res2 with
              | `Same -> same := (pb1,res1) :: !same
              | `LeftBetter -> regressed := (pb1, res1, res2) :: !regressed
              | `RightBetter -> improved := (pb1, res1, res2) :: !improved
              | `Mismatch -> mismatch := (pb1, res1, res2) :: !mismatch
          end;
          None
        ) a b
      );
    { appeared= !appeared; disappeared= !disappeared; mismatch= !mismatch;
      improved= !improved; regressed= !regressed; same= !same; }

  let fpf = Format.fprintf
  let pp_list_ p = Format.pp_print_list p
  let pp_hvlist_ p out = fpf out "[@[<hv>%a@]]" (pp_list_ p)
  let pp_pb_res out (pb,res) = fpf out "@[<h>%s: %a@]" pb.Problem.name Res.print res
  let pp_pb_res2 ~bold ~color out (pb,res1,res2) =
    let module F = FrogMisc.Fmt in
    fpf out "@[<h>%s: %a@]" pb.Problem.name
      ((if bold then F.in_bold_color color else F.in_color color)
        (fun out () -> fpf out "%a -> %a" Res.print res1 Res.print res2))
      ()

  (* TODO: colors! *)
  let print out t =
    let module F = FrogMisc.Fmt in
    fpf out "@[<v2>comparison: {@,appeared: %a,@ disappeared: %a,@ same: %a \
      ,@ mismatch: %a,@ improved: %a,@ regressed: %a@]@,}"
      (pp_hvlist_ pp_pb_res) t.appeared
      (pp_hvlist_ pp_pb_res) t.disappeared
      (pp_hvlist_ pp_pb_res) t.same
      (pp_hvlist_ (pp_pb_res2 ~bold:true ~color:`Red)) t.mismatch (* RED *)
      (pp_hvlist_ (pp_pb_res2 ~bold:false ~color:`Green)) t.improved (* GREEN *)
      (pp_hvlist_ (pp_pb_res2 ~bold:true ~color:`Yellow)) t.regressed (* YELLOW *)
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
let run_pb_ ~config pb =
  Lwt_log.ign_debug_f "running %-30s..." pb.Problem.name;
  (* spawn process *)
  let%lwt (out,_err,errcode) = Prover.run_proc
    ~timeout:config.Config.timeout
    ~memory:config.Config.memory
    ~prover:config.Config.prover
    ~file:pb.Problem.name
    ()
  in
  Lwt_log.ign_debug_f "output for %s: `%s`, `%s`, errcode %d"
    pb.Problem.name out _err errcode;
  (* parse its output *)
  let actual = extract_res_ ~prover:config.Config.prover out errcode in
  Lwt.return actual

let run_pb ?(caching=true) ?limit ~config pb =
  let module V = Maki.Value in
  Maki.call_exn
    ?limit
    ~bypass:(not caching)
    ~lifetime:(`KeepFor Maki.Time.(days 2))
    ~deps:[V.pack Config.maki config; V.pack Problem.maki pb]
    ~op:Res.maki
    ~name:"frogtest.run_pb"
    (fun () -> run_pb_ ~config pb)

let nop2_ _ _ = Lwt.return_unit

let run ?(on_solve = nop2_) ?(caching=true) ?j ?timeout ?memory ~config set =
  let config = Config.update ?j ?timeout ?memory config in
  let limit = Maki.Limit.create config.Config.j in
  let%lwt raw =
    Lwt_list.map_p
      (fun pb ->
         let%lwt res = run_pb ~caching ~limit ~config pb in
         let%lwt () = on_solve pb res in
         Lwt.return (pb, res)
      )
      set
  in
  Lwt.return (Results.of_list raw)

