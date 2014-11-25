
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
    unsat : string; (* regex for "unsat" *)
    sat : string;
    unknown : string option;
    timeout : string option;
  }

  (* command ready to run in a shell *)
  let make_command p ~timeout ~memory ~file =
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
    let unsat = Conf.get_string d "unsat" in
    let sat = Conf.get_string d "sat" in
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
  let tptp = Conf.get_string ~default:"" config "TPTP" in
  let timeout = match timeout with
    | Some t -> t
    | None -> Conf.get_int ~default:30 config "timeout"
  in
  let memory = match memory with
    | Some m -> m
    | None -> Conf.get_int ~default:1000 config "memory"
  in
  debug "tptp: %s, timeout: %d, memory: %d" tptp timeout memory;
  let cmd = Prover.make_command p ~timeout ~memory ~file in
  let cmd = if tptp="" then cmd else "TPTP="^tptp ^ " " ^  cmd in
  let cmd = ["/bin/sh"; "-c"; cmd] in
  debug "run command '%s'" (String.concat " " cmd);
  Unix.execv "/bin/sh" (Array.of_list cmd)

(* analyse one file, obtained from given prover *)
let analyse_single prover job results =
  let num_all = StrMap.cardinal results in
  let re_sat = Re.compile (Re.no_case (Re_posix.re prover.Prover.sat)) in
  let re_unsat = Re.compile (Re.no_case (Re_posix.re prover.Prover.unsat)) in
  let set_sat = Hashtbl.create 32 in
  let set_unsat = Hashtbl.create 32 in
  let num_error = ref 0 in
  let total_time = ref 0. in
  StrMap.iter
    (fun file res ->
      total_time := !total_time +. res.St.res_time;
      if res.St.res_errcode <> 0
        then incr num_error;
      if Re.execp re_sat res.St.res_out
        then Hashtbl.add set_sat file ()
      else if Re.execp re_unsat res.St.res_out
        then Hashtbl.add set_unsat file ()
      else ()
    ) results;
  let num_sat = Hashtbl.length set_sat in
  let num_unsat = Hashtbl.length set_unsat in
  let percent_solved = (float (num_sat + num_unsat) *. 100. /. float num_all) in
  Printf.printf "%d sat, %d unsat, %d total (%.0f%% solved) in %.2fs, %d errors\n"
    num_sat num_unsat num_all percent_solved !total_time !num_error;
  ()

let analyse ~config prover file =
  debug "analyse file %s, obtained from prover %s" file prover;
  let p = Prover.find_config config prover in
  (* obtain the full list of problems deal with *)
  let job, results = St.fold_state
    (fun (job,map) res ->
      job, StrMap.add res.St.res_arg res map
    ) (fun job -> job, StrMap.empty) file
    |> Lwt_main.run
  in
  analyse_single p job results

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
  | Analyse of string * string  (* prover * file *)
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
  | Analyse (prover, file) ->
      analyse ~config prover file
  | ListProvers ->
      list_provers ~config

(** {2 Main} *)

(* TODO: analyse several files, compare them, etc. *)

let cmd_ = ref `NoCmd
let config_ = ref ["$HOME/.frogtptp.toml"; (* "/etc/frogtptp.toml" *) ]
let args_ = ref []
let timeout_ = ref ~-1
let memory_ = ref ~-1

let set_analyse_ f =
  match split_comma f with
  | [p;file] -> cmd_ := `Analyse (p,file)
  | _ -> failwith "analyse: require a pair \"prover,file\""

let set_run_ p = cmd_ := `Run p
let push_config_ s = config_ := s :: !config_

let usage = "frogtptp cmd args"
let push_arg_ s = args_ := s :: !args_
let options = Arg.align
  [ "-analyse", Arg.String set_analyse_, " analyse given pair \"prover,output_file\""
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
  | `Analyse (prover,file), _ -> mk_params (Analyse (prover,file))
  | `Run prog, [arg] -> mk_params (Run (prog,arg))
  | `Run _, ([] | _::_::_)
  | `List, _ -> mk_params ListProvers
  | `NoCmd, _ ->
      Arg.usage options usage;
      exit 1
  in
  main params
