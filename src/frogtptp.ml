
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Manage TPTP provers} *)


(** {2 Description of a prover} *)

module Conf = FrogConfig
module StrMap = Map.Make(String)
module St = FrogMapState

let debug_ = ref false
let debug fmt =
  if !debug_
  then Printf.kfprintf
    (fun _ -> output_char stdout '\n'; flush stdout)
    stdout fmt
  else Printf.ifprintf stdout fmt

module Prover = struct
  type t = {
    cmd : string;  (* string that possible contains $file, $memory and $timeout *)
    unsat : string option; (* regex for "unsat" *)
    sat : string option;
    unknown : string option;
    timeout : string option;
  }

  (* command ready to run in a shell *)
  let make_command ?tptp p ~timeout ~memory ~file =
    let buf = Buffer.create 32 in
    let add_str s =
    Buffer.add_substitute buf
      (function
        | "memory" -> string_of_int memory
        | "timeout" | "time" -> string_of_int timeout
        | "file" -> file
        | _ -> raise Not_found
      ) s
    in
    add_str "ulimit -t $time -v \\$(( 1000000 * $memory )); ";
    begin match tptp with
      | None -> ()
      | Some s -> add_str ("TPTP="^ s ^ " ")
    end;
    add_str p.cmd;
    Buffer.contents buf

  (* recover description of prover from config file *)
  let build_from_config config name =
    let d =
      try Conf.get_table config name
      with Not_found ->
        failwith ("could not find prover " ^ name ^ " in config")
    in
    let cmd = Conf.get_string d "cmd" in
    let unsat = try Some (Conf.get_string d "unsat") with Not_found -> None in
    let sat = try Some (Conf.get_string d "sat") with Not_found -> None in
    let unknown = try Some (Conf.get_string d "unknown") with Not_found -> None in
    let timeout = try Some (Conf.get_string d "timeout") with Not_found -> None in
    { cmd; unsat; sat; unknown; timeout; }

  let find_config config name =
    (* check that the prover is listed *)
    let provers = Conf.get_string_list ~default:[] config "provers" in
    if not (List.mem name provers)
      then failwith ("prover " ^ name ^ " not listed in config");
    build_from_config config name

  (* make a list of provers from the given config *)
  let of_config config =
    let provers = Conf.get_string_list ~default:[] config "provers" in
    List.fold_left
      (fun map p_name ->
        let prover = build_from_config config p_name in
        StrMap.add p_name prover map
      ) StrMap.empty provers
end

let re_not_comma = Re_posix.compile_pat "[^,]+"

(* split a string along "," *)
let split_comma s =
  let l = ref [] in
  let i = ref 0 in
  try
    while !i < String.length s do
      let sub = Re.exec ~pos:!i re_not_comma s in
      l := Re.get sub 0 :: !l;
      let start, len = Re.get_ofs sub 0 in
      i := start + len;
    done;
    List.rev !l
  with Not_found ->
    List.rev !l

let run ?timeout ?memory ~config prog file =
  let p = Prover.find_config config prog in
  let tptp = match Conf.get_string ~default:"" config "TPTP" with
    | "" -> None
    | s -> Some s
  in
  let timeout = match timeout with
    | Some t -> t
    | None -> Conf.get_int ~default:30 config "timeout"
  in
  let memory = match memory with
    | Some m -> m
    | None -> Conf.get_int ~default:1000 config "memory"
  in
  debug "tptp: %s, timeout: %d, memory: %d" ([%show:string option] tptp) timeout memory;
  let cmd = Prover.make_command ?tptp p ~timeout ~memory ~file in
  let cmd = ["/bin/sh"; "-c"; cmd] in
  debug "run command '%s'" (String.concat " " cmd);
  Unix.execv "/bin/sh" (Array.of_list cmd)

type file_summary = {
  mutable num_all : int;
  mutable set_unsat : float StrMap.t;  (* pb -> time *)
  mutable set_sat : float StrMap.t; (* pb -> time *)
  mutable num_error : int;
  mutable total_time : float; (* of successfully solved problems *)
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

module Opt = struct
  let (>>=) o f = match o with
    | None -> None
    | Some x -> f x
end

(* compute summary of this file *)
let make_summary prover job results =
  let s = {
    num_all=List.length job.St.arguments;
    set_unsat=StrMap.empty;
    set_sat=StrMap.empty;
    num_error=0;
    total_time=0.;
  } in
  let re_sat = Opt.(prover.Prover.sat >>= compile_re ~msg:"sat") in
  let re_unsat = Opt.(prover.Prover.unsat >>= compile_re ~msg:"unsat") in
  let re_unknown = Opt.(prover.Prover.unknown >>= compile_re ~msg:"unknown") in
  StrMap.iter
    (fun file res ->
      if res.St.res_errcode <> 0
        then s.num_error <- s.num_error + 1;
      if execp_re_maybe re_sat res.St.res_out then (
        s.total_time <- s.total_time +. res.St.res_time;
        s.set_sat <- StrMap.add file res.St.res_time s.set_sat
      ) else if execp_re_maybe re_unsat res.St.res_out then (
        s.total_time <- s.total_time +. res.St.res_time;
        s.set_unsat <- StrMap.add file res.St.res_time s.set_unsat
      );
    ) results;
  s

module PB = PrintBox

let print_single_summary prover s =
  let num_sat = StrMap.cardinal s.set_sat in
  let num_unsat = StrMap.cardinal s.set_unsat in
  let percent_solved = (float (num_sat + num_unsat) *. 100. /. float s.num_all) in
  Printf.printf "prover %s: %d sat, %d unsat, %d total (%.0f%% solved) in %.2fs, %d errors\n"
    prover num_sat num_unsat s.num_all percent_solved s.total_time s.num_error;
  ()

(* obtain the full list of problems/results deal with in this file *)
let extract_file file =
  St.fold_state
    (fun (job,map) res ->
      job, StrMap.add res.St.res_arg res map
    ) (fun job -> job, StrMap.empty) file
    |> Lwt_main.run

let analyse_single_file ~config prover file =
  let p = Prover.find_config config prover in
  let job, results = extract_file file in
  let s = make_summary p job results in
  print_single_summary prover s

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
      | _ -> None
    ) m1 m2

(* analyse and compare this list of prover,job,results *)
let analyse_multiple items =
  (* individual results *)
  let map = List.fold_left
    (fun map (p_name,p,job,results) ->
      let summary = make_summary p job results in
      (* individual summary *)
      StrMap.add p_name summary map
    ) StrMap.empty items
  in
  (* some globals *)
  let problems_solved_by_one = ref StrMap.empty in
  (* print overall *)
  let first_line = PB.(
    [| text "prover"; text "sat"; text "unsat"; text "total"; text "exclusive";
       text "%solved"; text "time (s)"; text "avg time (s)"; text "errors" |]
  ) in
  (* next lines *)
  let next_lines = StrMap.fold
    (fun prover s acc ->
      let problems_solved_by_me = all_proved s in
      let problems_solved_by_others = all_proved_map (StrMap.remove prover map) in
      let problems_solved_by_me_only = map_diff
        problems_solved_by_me problems_solved_by_others
      in
      problems_solved_by_one := StrMap.add prover
        problems_solved_by_me_only !problems_solved_by_one;
      let num_sat = StrMap.cardinal s.set_sat in
      let num_unsat = StrMap.cardinal s.set_unsat in
      let percent_solved = (float (num_sat + num_unsat) *. 100. /. float s.num_all) in
      let num_solved_only = StrMap.cardinal problems_solved_by_me_only in
      PB.([| text prover; int_ num_sat; int_ num_unsat; int_ s.num_all;
             int_ num_solved_only;
             text (Printf.sprintf "%.0f" percent_solved);
             text (Printf.sprintf "%.2f" s.total_time);
             text (Printf.sprintf "%.2f" (s.total_time /. float s.num_all));
             int_ s.num_error |]
      ) :: acc
    ) map []
  in
  let box = PB.(frame (grid (Array.of_list (first_line :: next_lines)))) in
  print_endline "";
  PB.output stdout box;
  print_endline "";
  (* TODO: pairwise comparison *)
  (* print, for each prover, list of problems it's the only one to solve *)
  StrMap.iter
    (fun prover map ->
      Printf.printf "problems solved by only %s:\n" prover;
      StrMap.iter
        (fun file () -> Printf.printf "  %s\n" file)
        map
    ) !problems_solved_by_one;
  ()

let analyse_multiple_files ~config l =
  let items = List.map
    (fun (prover,file) ->
      let p = Prover.find_config config prover in
      let job, results = extract_file file in
      prover, p, job, results
    ) l
  in
  analyse_multiple items

let analyse ~config l = match l with
  | [] -> assert false
  | [p, file] ->
      debug "analyse file %s, obtained from prover %s" file p;
      analyse_single_file ~config p file
  | _ ->
      debug "analyse %d files" (List.length l);
      analyse_multiple_files ~config l

(* print list of known provers *)
let list_provers ~config =
  let provers = Prover.of_config config in
  Printf.printf "provers:\n";
  StrMap.iter
    (fun name p ->
      Printf.printf "  %s: cmd=%s\n" name p.Prover.cmd;
    ) provers

(** {2 Run} *)

type cmd =
  | Analyse of (string * string) list  (* list (prover, file) *)
  | Run of string * string
  | ListProvers

type specific_conf = {
  timeout : int option;
  memory : int option;
}

type params = {
  cmd : cmd;
  config_files : string list;
  conf : specific_conf;
}

let main params =
  let config = FrogConfig.parse_files params.config_files FrogConfig.empty in
  match params.cmd with
  | Run (prog,args) ->
      run
        ?timeout:params.conf.timeout
        ?memory:params.conf.memory
        ~config prog args
  | Analyse l ->
      analyse ~config l
  | ListProvers ->
      list_provers ~config

(** {2 Main} *)

let cmd_ = ref `NoCmd
let config_ = ref ["$HOME/.frogtptp.toml"; (* "/etc/frogtptp.toml" *) ]
let args_ = ref []
let timeout_ = ref ~-1
let memory_ = ref ~-1

(* TODO: only one -analyse flag, but with a "prover=file,prover=file,..." argument *)

let add_analyse_ x =
  match !cmd_ with
  | `Analyse l -> cmd_ := `AnalyseFst (x, l)
  | `AnalyseFst (p, l) -> cmd_ := `Analyse( (p,x) :: l)
  | `NoCmd -> cmd_ := `AnalyseFst (x, [])
  | `Run _ -> failwith "-analyse must not be used with -run"
  | `List -> failwith "-analyse must not be used with -list"

let set_run_ p = cmd_ := `Run p
let push_config_ s = config_ := s :: !config_

let usage = "frogtptp cmd args"
let push_arg_ s = args_ := s :: !args_
let options = Arg.align
  [ "-analyse", Arg.Rest add_analyse_, " analyse pairs of \"prover\" \"output_file\""
  ; "-run", Arg.String set_run_, " run given prover"
  ; "-config", Arg.String push_config_, " use given config file"
  ; "-list", Arg.Unit (fun () -> cmd_ := `List), " list provers"
  ; "-memory", Arg.Set_int memory_, " memory limit (MB)"
  ; "-timeout", Arg.Set_int timeout_, " timeout (s)"
  ; "-debug", Arg.Set debug_, " enable debug"
  ; "--", Arg.Rest push_arg_, " stop parsing arguments"
  ]

let some_if_pos_ i =
  if i>0 then Some i else None

let () =
  Arg.parse options push_arg_ usage;
  let config_files = List.map Conf.interpolate_home !config_ in
  let conf = {memory=some_if_pos_ !memory_; timeout= some_if_pos_ !timeout_;} in
  let mk_params cmd = {cmd; config_files; conf; } in
  let params = match !cmd_, List.rev !args_ with
  | `Analyse l, _ -> mk_params (Analyse l)
  | `Run prog, [arg] -> mk_params (Run (prog,arg))
  | `Run _, ([] | _::_::_)
  | `List, _ -> mk_params ListProvers
  | `AnalyseFst _, _
  | `NoCmd, _ ->
      Arg.usage options usage;
      exit 1
  in
  main params
