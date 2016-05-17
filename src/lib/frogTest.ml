
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Tools to test a prover} *)

type 'a or_error = [`Ok of 'a | `Error of string]
type 'a printer = Format.formatter -> 'a -> unit
type html = FrogWeb.html
type uri = FrogWeb.uri

module MStr = Map.Make(String)
module Prover = FrogProver

let fpf = Format.fprintf
let spf = Format.asprintf

let assoc_or def x l =
  try List.assoc x l
  with Not_found -> def

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

  let to_string = function
    | Sat -> "sat"
    | Unsat -> "unsat"
    | Unknown -> "unknown"
    | Error -> "error"

  let print out s = Format.pp_print_string out (to_string s)

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

  let to_html s = FrogWeb.Html.string (to_string s)
end

module Problem = struct
  [@@@warning "-39"]
  type t = {
    name: string;  (* filename *)
    expected: Res.t; (* result expected *)
  } [@@deriving yojson]
  [@@@warning "+39"]

  let same_name t1 t2 = t1.name = t2.name
  let compare_name t1 t2 = Pervasives.compare t1.name t2.name

  (* regex + mark *)
  let m_unsat_, unsat_ = Re.(str "unsat" |> no_case |> mark)
  let m_sat_, sat_ = Re.(str "sat" |> no_case |> mark)
  let m_unknown_, unknown_ = Re.(str "unknown" |> no_case |> mark)
  let m_error_, error_ = Re.(alt [str "error"; str "fail"] |> no_case |> mark)

  (* "^ #expect: (unsat|sat|unknown|error)", basically *)
  let re_expect_ = Re.(seq
    [ alt (List.map no_case [str "expect:"; str "expected:"]) ; rep blank;
      alt [unsat_; sat_; unknown_; error_] ]
    |> compile
  )

  (* what is expected? *)
  let find_expected_ ~file =
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
    | (Res.Sat | Res.Unsat | Res.Error), Res.Unknown -> `Disappoint
    | (Res.Unsat | Res.Error), Res.Sat
    | (Res.Sat | Res.Error), Res.Unsat
    | (Res.Sat | Res.Unknown | Res.Unsat), Res.Error ->
        `Mismatch
    | Res.Unknown, (Res.Sat | Res.Unsat) ->
        `Improvement

  let print out p =
    fpf out "@[<h>%s (expect: %a)@]" p.name Res.print p.expected

  let to_string p = FrogMisc.Fmt.to_string print p

  let maki =
    let module V = Maki.Value in
    V.map ~descr:"problem"
      (fun p -> p.name, p.expected)
      (fun (name, expected) -> {name;expected})
      (V.pair V.file Res.maki)

  let to_html_name p = FrogWeb.Html.string p.name
  let to_html_full p = FrogWeb.Html.string (to_string p)
end

module ProblemSet = struct
  type t = Problem.t list

  let make l =
    let pool = Lwt_pool.create 30 (fun () -> Lwt.return_unit) in
    let%lwt l =
      Lwt_list.map_p
        (fun file ->
          Lwt_pool.use pool
            (fun () -> Problem.make ~file))
        l
    in
    let l = FrogMisc.Err.seq_list l in
    (* sort by alphabetic order *)
    let l = FrogMisc.Err.(l >|= List.sort Problem.compare_name) in
    Lwt.return l

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

  let to_html uri_of_pb l =
    let module H = FrogWeb.Html in
    let f pb = H.a ~href:(uri_of_pb pb) (Problem.to_html_name pb) in
    H.div ~attrs:["class", "problem_set"]
      (H.list (List.map f l))
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

  let to_html uri_of_prover c =
    let module H = FrogWeb.Html in
    let module R = FrogWeb.Record in
    R.start
    |> R.add_int "j" c.j
    |> R.add_int "timeout" c.timeout
    |> R.add_int "memory" c.memory
    |> R.add_string "problems pattern" c.problem_pat
    |> R.add "prover"
      (H.a ~href:(uri_of_prover c.prover) (Prover.to_html_name c.prover))
    |> R.close
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
    disappoint: (Problem.t * Res.t) list;
    mismatch: (Problem.t * Res.t) list;
  }

  let pp_pb_res_ out (pb, res) =
    fpf out "@[<h>problem %s (expected: %a, result: %a)@]"
      pb.Problem.name Res.print pb.Problem.expected Res.print res

  let pp_list_ p = Format.pp_print_list p

  (* build statistics and list of mismatch from raw results *)
  let analyse_ raw =
    let module M = OLinq.AdaptMap(MStr) in
    let l =
      M.of_map raw
      |> OLinq.map snd
      |> OLinq.group_by
        (fun (pb, res) -> Problem.compare_res pb res)
      |> OLinq.run_list ?limit:None
    in
    let improved = assoc_or [] `Improvement l in
    let mismatch = assoc_or [] `Mismatch l in
    let disappoint = assoc_or [] `Disappoint l in
    (* stats *)
    let stat = ref stat_empty in
    let add_res res =
      stat := (match res with
        | Res.Unsat -> add_unsat_ | Res.Sat -> add_sat_
        | Res.Unknown -> add_unknown_ | Res.Error -> add_error_
      ) !stat
    in
    MStr.iter (fun _ (_, res) -> add_res res) raw;
    improved, mismatch, disappoint, !stat

  let add_raw raw pb res =
    MStr.add pb.Problem.name (pb,res) raw

  let make raw =
    let improved, mismatch, disappoint, stat = analyse_ raw in
    { raw; stat; improved; disappoint; mismatch; }

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
    let pp_l out = fpf out "[@[<hv>%a@]]" (pp_list_ pp_pb_res_) in
    fpf out
      "@[<hv2>results: {@,stat:%a,@ %-15s: %a,@ %-15s: %a,@ %-15s: %a@]@,}"
      pp_stat r.stat
      "improved" pp_l r.improved
      "disappoint" pp_l r.disappoint
      "mismatch" pp_l r.mismatch

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

  let to_html_raw uri_of_problem r =
    let module H = FrogWeb.Html in
    let l = MStr.fold (fun _ (pb,res) acc -> (pb,res)::acc) r [] in
    H.Create.table ~flags:[H.Create.Tags.Headings_fst_row]
      ~row:(fun (pb,res) ->
        [ H.a ~href:(uri_of_problem pb) (Problem.to_html_name pb)
        ; Res.to_html res
        ])
      l

  (* TODO: print tables imrpoved/disappoint, then, lower, print raw *)
  let to_html uri_of_problem t =
    to_html_raw uri_of_problem t.raw
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

  (* TODO: use outer_join? to also find the disappeared/appeared *)
  let compare (a:Results.raw) b =
    let module M = OLinq.AdaptMap(MStr) in
    let a = M.of_map a |> OLinq.map snd in
    let b = M.of_map b |> OLinq.map snd in
    let j =
      OLinq.join ~eq:Problem.same_name fst fst a b
        ~merge:(fun pb (pb1,res1) (pb2,res2) ->
          assert (pb1.Problem.name = pb2.Problem.name);
          Some (pb, res1, res2))
      |> OLinq.group_by (fun (_,res1,res2) -> Res.compare res1 res2)
      |> OLinq.run_list
    in
    let improved = assoc_or [] `RightBetter j in
    let regressed = assoc_or [] `LeftBetter j in
    let mismatch = assoc_or [] `Mismatch j in
    let same = assoc_or [] `Same j |> List.rev_map (fun (pb,r,_) -> pb,r) in
    let disappeared =
      OLinq.diff ~cmp:(fun (p1,_) (p2,_) -> Problem.compare_name p1 p2) a b
      |> OLinq.run_list
    and appeared =
      OLinq.diff ~cmp:(fun (p1,_) (p2,_) -> Problem.compare_name p1 p2) b a
      |> OLinq.run_list
    in
    { appeared; disappeared; mismatch; same; regressed; improved; }

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

  let to_html _ _ = assert false (* TODO! *)
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
         Lwt.return (pb, res))
      set
  in
  Lwt.return (Results.of_list raw)

