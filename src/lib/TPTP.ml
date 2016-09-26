
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Manage TPTP provers} *)

module St = MapState
module Conf = Config
module PB = PrintBox
module Opt = Misc.Opt
module StrMap = Misc.StrMap

(** {2 Type Definitions} *)

(* TODO: have a module to define times and operations on them ? *)
type time = {
  real : float;
  user : float;
  system : float;
}

type file_summary = {
  prover : string;
  mutable num_all : int;
  mutable set_unsat : time Misc.StrMap.t;  (* pb -> time *)
  mutable set_sat : time Misc.StrMap.t; (* pb -> time *)
  mutable num_error : int;
  mutable solved_time : time; (* of successfully solved problems *)
  mutable run_time : time; (* total run time of the prover *)
}

type run_params = {
  timeout : int option;
  memory : int option;
}

type analyse_params = {
  get_time : time -> float;
  filter : file_summary Misc.StrMap.t -> string -> St.result -> bool;
}

let compile_re ~msg re =
  try
    Some (Re.compile (Re.no_case (Re_posix.re re)))
  with e ->
    Printf.eprintf "could not compile regex %s: %s"
      msg (Printexc.to_string e);
    None

let execp_re_maybe maybe_re s = match maybe_re with
  | None -> false
  | Some re -> Re.execp re s

let add_time t t' = {
  real = t.real +. t'.real;
  user = t.user +. t'.user;
  system = t.system +. t'.system;
}

let time_of_res res = {
  real = res.St.res_rtime;
  user = res.St.res_utime;
  system = res.St.res_stime;
}

(* compute summary of this file *)
let make_summary ?(filter=(fun _ _ -> true)) prover_name prover job results =
  let s = {
    prover = prover_name;
    num_all = List.length job.St.arguments;
    set_unsat = Misc.StrMap.empty;
    set_sat = Misc.StrMap.empty;
    num_error = 0;
    solved_time = { real = 0.; user = 0.; system = 0.; };
    run_time = { real = 0.; user = 0.; system = 0.; };
  } in
  let re_sat = Opt.(prover.Prover.sat >>= compile_re ~msg:"sat") in
  let re_unsat = Opt.(prover.Prover.unsat >>= compile_re ~msg:"unsat") in
  (*
  let re_unknown = Opt.(prover.Prover.unknown >>= compile_re ~msg:"unknown") in
  *)
  StrMap.iter
    (fun file res ->
       if filter file res then begin
         let time = time_of_res res in
         s.run_time <- add_time s.run_time time;
         if res.St.res_errcode <> 0 then s.num_error <- s.num_error + 1;
         if execp_re_maybe re_sat res.St.res_out then begin
           s.solved_time <- add_time s.solved_time time;
           s.set_sat <- StrMap.add file time s.set_sat
         end else if execp_re_maybe re_unsat res.St.res_out then begin
           s.solved_time <- add_time s.solved_time time;
           s.set_unsat <- StrMap.add file time s.set_unsat
         end
       end)
    results;
  s

let map_summaries params items =
  let aux ?filter l = List.fold_left
      (fun map (file,p_name,p,job,results) ->
         let summary = make_summary ?filter p_name p job results in
         StrMap.add file summary map)
      StrMap.empty l
  in
  let map = aux items in
  aux ~filter:(params.filter map) items

(* obtain the full list of problems/results deal with in this file *)
let extract_file file =
  St.fold_state
    (fun (job,map) res ->
       job, StrMap.add res.St.res_arg res map)
    (fun job -> job, StrMap.empty) file
    |> Lwt_main.run

(* Single summary analysis *)
let print_file_summary out s =
  let num_sat = StrMap.cardinal s.set_sat in
  let num_unsat = StrMap.cardinal s.set_unsat in
  let percent_solved = (float (num_sat + num_unsat) *. 100. /. float s.num_all) in
  Printf.fprintf out
    "prover: %s, %d sat, %d unsat, %d total (%.0f%% solved), %d errors\n  \
     solved time : %.2fs (real), %.2f (user), %.2f (system)\n  \
     total run time : %.2fs (real), %.2f (user), %.2f (system)\n"
    s.prover num_sat num_unsat s.num_all percent_solved s.num_error
    s.solved_time.real s.solved_time.user s.solved_time.system
    s.run_time.real s.run_time.user s.run_time.system;
  ()

let analyse_single_file ~config prover file =
  let p = Prover.find_config config prover in
  let job, results = extract_file file in
  make_summary prover p job results

let all_proved s =
  StrMap.merge (fun _ _ _ -> Some ()) s.set_unsat s.set_sat

(* input: map prover -> summary
  output: map problem -> unit, containing every problem solved by at
      least one prover *)
let all_proved_map map =
  try
    let min_p, s = StrMap.min_binding map in
    let acc = all_proved s in
    let others = StrMap.remove min_p map in
    StrMap.fold
      (fun _ s acc ->
        acc
        |> StrMap.merge (fun _ _ _ -> Some ()) s.set_unsat
        |> StrMap.merge (fun _ _ _ -> Some ()) s.set_sat
      ) others acc
  with Not_found ->
    StrMap.empty

let map_diff m1 m2 =
  StrMap.merge
    (fun _ x1 x2 -> match x1, x2 with
      | Some x, None -> Some x
      | _ -> None)
    m1 m2

type analysis_result = {
  ar_file: string;
  ar_prover: string;
  ar_sat: int;
  ar_unsat: int;
  ar_total: int;
  ar_num_exclusive: int;
  ar_exclusive: unit StrMap.t; (* set of files solved only by this prover *)
  ar_percent_solved: float;
  ar_time: float;
  ar_avg_time: float;
  ar_errors: int;
  ar_runtime: float;
}

(* TODO: use Olinq/OLinq_table? *)
(* analyse and compare this list of prover,job,results *)
let analyse_multiple params items =
  let map = map_summaries params items in
  (* some globals *)
  let problems_solved_by_one = ref StrMap.empty in
  StrMap.fold
    (fun file s acc ->
       let ar_prover = s.prover in
       let problems_solved_by_me = all_proved s in
       let problems_solved_by_others = all_proved_map (StrMap.remove file map) in
       let problems_solved_by_me_only = map_diff
           problems_solved_by_me problems_solved_by_others
       in
       problems_solved_by_one := StrMap.add ar_prover
           problems_solved_by_me_only !problems_solved_by_one;
       let ar_sat = StrMap.cardinal s.set_sat in
       let ar_unsat = StrMap.cardinal s.set_unsat in
       let ar_percent_solved = (float (ar_sat + ar_unsat) *. 100. /. float s.num_all) in
       let ar_exclusive = problems_solved_by_me_only in
       let ar_num_exclusive = StrMap.cardinal ar_exclusive in
       let ar_time = params.get_time s.solved_time in
       let ar_runtime = params.get_time s.run_time in
       let ar_avg_time = ar_time /. float (ar_sat + ar_unsat) in
       let ar = {
         ar_sat; ar_unsat; ar_percent_solved; ar_exclusive; ar_num_exclusive;
         ar_prover; ar_file=file; ar_total=s.num_all; ar_errors=s.num_error;
         ar_time; ar_runtime; ar_avg_time;
       } in
       ar :: acc)
    map []
  |> List.rev (* keep ascending order of file names *)

(* TODO: pairwise comparison of exclusive problems (using OLinq) *)

let box_of_ar ar_l =
  (* print overall *)
  let first_line = PB.(
      [| text "file"; text "prover"; text "sat"; text "unsat"; text "total"; text "exclusive";
         text "%solved"; text "time (s)"; text "avg time (s)"; text "errors" ; text "runtime (s)" |]
  ) in
  let next_lines =
    List.map
      (fun ar ->
         PB.(
           [| text @@ Filename.basename ar.ar_file; text ar.ar_prover;
              int_ ar.ar_sat; int_ ar.ar_unsat; int_ ar.ar_total;
              int_ ar.ar_num_exclusive;
              text (Printf.sprintf "%.0f" ar.ar_percent_solved);
              text (Printf.sprintf "%.2f" ar.ar_time);
              text (Printf.sprintf "%.2f" ar.ar_avg_time);
              int_ ar.ar_errors;
              text (Printf.sprintf "%.2f" ar.ar_runtime) |]
         ))
      ar_l
  in
  PB.(frame (grid (Array.of_list (first_line :: next_lines))))

let print_ar_exclusive out ar =
  (* print, for each prover, list of problems it's the only one to solve *)
  Printf.fprintf out "problems solved by only %s:\n" ar.ar_prover;
  StrMap.iter
    (fun file () -> Printf.printf "  %s\n" file)
    ar.ar_exclusive;
  ()

let parse_prover_list ~config l =
  List.map
    (fun (prover,file) ->
       let p = Prover.find_config config prover in
       let job, results = extract_file file in
       file, prover, p, job, results)
    l

