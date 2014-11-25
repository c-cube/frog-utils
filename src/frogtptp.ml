
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
    add_str "ulimit -t $time -m $memory; ";
    add_str p.cmd;
    Buffer.contents buf

  (* make a list of provers from the given config *)
  let of_config config =
    let provers = Conf.get_string_list ~default:[] config "provers" in
    List.fold_left
      (fun map p_name ->
        let d =
          try Conf.get_table config p_name
          with Not_found ->
            failwith ("could not find prover " ^ p_name ^ " in config")
        in
        let cmd = Conf.get_string d "cmd" in
        let unsat = Conf.get_string d "unsat" in
        let sat = Conf.get_string d "sat" in
        let unknown = try Some (Conf.get_string d "unknown") with Not_found -> None in
        let timeout = try Some (Conf.get_string d "timeout") with Not_found -> None in
        StrMap.add p_name { cmd; unsat; sat; unknown; timeout; } map
      ) StrMap.empty provers
end

let run ~config prog file =
  let provers = Prover.of_config config in
  let timeout = Conf.get_int ~default:30 config "timeout" in
  let memory = Conf.get_int ~default:1000 config "memory" in
  try
    let p = StrMap.find prog provers in
    let cmd = [| "/bin/sh"; "-c"; Prover.make_command p ~timeout ~memory ~file |] in
    Unix.execv "/bin/sh" cmd
  with Not_found ->
    failwith ("could not find description of prover " ^ prog)

let analyse ~config file =
  assert false (* TODO: use FrogMapState to analyse results *)

let list_provers ~config =
  let provers = Prover.of_config config in
  Printf.printf "provers:\n";
  StrMap.iter
    (fun name p ->
      Printf.printf "  %s: cmd=%s\n" name p.Prover.cmd;
    ) provers

(** {2 Run} *)

type cmd =
  | Analyse of string
  | Run of string * string
  | ListProvers

type params = {
  cmd : cmd;
  config_files : string list;
}

let main params =
  let config = FrogConfig.parse_files params.config_files FrogConfig.empty in
  match params.cmd with
  | Run (prog,args) ->
      run ~config prog args
  | Analyse file ->
      analyse ~config file
  | ListProvers ->
      list_provers ~config

(** {2 Main} *)

(* TODO: analyse several files, compare them, etc. *)

let cmd_ = ref `NoCmd
let config_ = ref ["$HOME/.frogtptp.toml"; (* "/etc/frogtptp.toml" *) ]
let args_ = ref []

let set_analyse_ f = cmd_ := `Analyse f
let set_run_ p = cmd_ := `Run p
let push_config_ s = config_ := s :: !config_

let usage = "frogtptp cmd args"
let push_arg_ s = args_ := s :: !args_
let options = Arg.align
  [ "-analyse", Arg.String set_analyse_, " analyse given output file"
  ; "-run", Arg.String set_run_, " run given prover"
  ; "-config", Arg.String push_config_, " use given config file"
  ; "-list", Arg.Unit (fun () -> cmd_ := `List), " list provers"
  ; "--", Arg.Rest push_arg_, " stop parsing arguments"
  ]

let () =
  Arg.parse options push_arg_ usage;
  let config_files = List.map Conf.interpolate_home !config_ in
  let mk_params cmd = {cmd; config_files; } in
  let params = match !cmd_, List.rev !args_ with
  | `Analyse f, _ -> mk_params (Analyse f)
  | `Run prog, [arg] -> mk_params (Run (prog,arg))
  | `Run _, ([] | _::_::_)
  | `List, _ -> mk_params ListProvers
  | `NoCmd, _ ->
      Arg.usage options usage;
      exit 1
  in
  main params
