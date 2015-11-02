
(*
copyright (c) 2013-2015, simon cruanes
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

(** {1 Deal with TPTP provers} *)

module Conf = FrogConfig
module StrMap = Map.Make(String)
module St = FrogMapState

module Prover = struct
  type t = {
    cmd : string;  (* string that possible contains $file,
                      $memory and $timeout *)
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
    add_str "ulimit -t \\$(( 1 + $time)) -v \\$(( 1000000 * $memory )); ";
    begin match tptp with
      | None -> ()
      | Some s -> add_str ("TPTP="^ s ^ " ")
    end;
    add_str p.cmd;
    Buffer.contents buf

  let get_str_ d x =
    try Some (Conf.get_string d x)
    with Not_found -> None

  (* recover description of prover from config file *)
  let build_from_config config name =
    let d =
      try Conf.get_table config name
      with Not_found ->
        failwith ("could not find prover " ^ name ^ " in config")
    in
    let cmd = Conf.get_string d "cmd" in
    let unsat = get_str_ d "unsat" in
    let sat = get_str_ d "sat" in
    let unknown = get_str_ d "unknown" in
    let timeout = get_str_ d "timeout" in
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

let run_cmd ?timeout ?memory ~config ~prover ~file =
  let p = Prover.find_config config prover in
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
  FrogDebug.debug "tptp: %s, timeout: %d, memory: %d"
    ([%show:string option] tptp) timeout memory;
  let cmd = Prover.make_command ?tptp p ~timeout ~memory ~file in
  let cmd = ["/bin/sh"; "-c"; cmd] in
  "/bin/sh", Array.of_list cmd

let run_exec ?timeout ?memory ~config ~prover ~file () =
  let cmd, args = run_cmd ?timeout ?memory ~config ~prover ~file in
  Unix.execv cmd args
