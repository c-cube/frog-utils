
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


(** {1 Call a command on each result of frogmap} *)

module S = FrogMapState

(* what kind of input to give to the command, for every result? *)
type format_in =
  | OutOnly  (* pass result.res_out to command *)
  | Prelude  (* pass result.res_out + prelude with other data *)
  | AsArg    (* do not use cmd.stdin, but give result.res_out as CLI arg *)

type cmd =
  | Shell of string
  | Exec of string * string list
  | Stats
  [@@deriving show]

type params = {
  format_in : format_in;
  cmd : cmd;
  filename : string;
}

(* print metadata of result on the given chan *)
let print_prelude oc res =
  let%lwt () = Lwt_io.fprintf oc "# argument: %s\n" res.S.res_arg in
  Lwt_io.fprintf oc "# time: %.2f\n" res.S.res_time

let escape_quote s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '\'' -> Buffer.add_string buf "\'"
      | c -> Buffer.add_char buf c
    ) s;
  Buffer.contents buf

(* run command on the given result *)
let run_cmd params res =
  let cmd = match params.cmd with
    | Shell c ->
        Lwt_process.shell ("set -e; set -o pipefail; " ^ escape_quote c)
    | Exec (p, args)  -> (p, Array.of_list (p::args))
    | Stats -> assert false
  in
  (* give additional info through parameters *)
  let env = Unix.environment () |> Array.to_list in
  let env =
    [ "FROG_ERRCODE=" ^ res.S.res_err
    ; "FROG_TIME=" ^ string_of_float res.S.res_time
    ; "FROG_ERR=" ^ res.S.res_err
    ; "FROG_OUT=" ^ res.S.res_out
    ; "FROG_ARG=" ^ res.S.res_arg
    ] @ env
    |> Array.of_list
  in
  FrogDebug.debug "run sub-process %s" ([%show: string*string array] cmd);
  (* spawn process *)
  try%lwt
    Lwt_process.with_process_out
      ~stdout:`Keep ~stderr:`Keep ~env cmd
      (fun p ->
        let%lwt () = match params.format_in with
        | OutOnly ->
            Lwt_io.write p#stdin res.S.res_out
        | Prelude ->
            let%lwt () = print_prelude p#stdin res in
            Lwt_io.write p#stdin res.S.res_out
        | AsArg -> Lwt.return_unit
        in
        let%lwt () = Lwt_io.close p#stdin
        and _ = p#status in
        FrogDebug.debug "process finished";
        Lwt.return_unit
      )
  with e ->
    Lwt_io.eprintlf "error on command %s: %s"
      (show_cmd params.cmd) (Printexc.to_string e)

(* print some statistics *)
let show_stats filename =
  let%lwt (job, map) = S.read_state filename in
  let%lwt() = Lwt_io.printlf "job: run '%s' on %d arguments"
    job.S.cmd (List.length job.S.arguments) in
  (* compute basic statistics *)
  let foi = float_of_int in
  let num, sum_len_out = S.StrMap.fold
    (fun _ res (num,sum_len_out) ->
      num + 1, sum_len_out + String.length res.S.res_out
    ) map (0,0)
  in
  Lwt_io.printlf
    "%d arguments dealt with, total length of outputs %d (avg output len %.2f)"
    num sum_len_out (if num=0 then 0. else foi sum_len_out /. foi num)

let main params =
  match params.cmd with
  | Stats -> show_stats params.filename
  | Shell _
  | Exec _ ->
      FrogMapState.fold_state_s
        (fun () res ->
          run_cmd params res
        ) (fun _job -> Lwt.return_unit)
        params.filename

(** {2 Main} *)

let frogiter file cmd prelude arg debug shell stats =
  if debug then FrogDebug.enable_debug ();
  let format_in = if prelude then Prelude else if arg then AsArg else OutOnly in
  let mk_params filename cmd =
    {format_in = format_in; cmd; filename}
  in
  let params = match file, shell, cmd with
    | filename, _, _ when stats -> mk_params filename Stats
    | filename, Some cmd, _ ->
        mk_params filename (Shell cmd)
    | filename, None, prog::args ->
        mk_params filename (Exec (prog,args))
    | _ -> assert false
  in
  Lwt_main.run (main params)

let frogiter_t =
    let file =
        let doc = "Result file (typically, the output of frogmap)."in
        Cmdliner.Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)
    in
    let cmd =
        let doc = "Commandtorunon every output result in argument file" in
        Cmdliner.Arg.(non_empty & pos_right 0 string [] & info [] ~docv:"CMD"~doc)
    in
    let prelude =
        let doc = "Pipe additional info about the result into the command" in
        Cmdliner.Arg.(value & flag & info ["p"; "prelude"] ~doc)
    in
    let arg =
        let doc = "Give res.output only through environment, not stdin" in
        Cmdliner.Arg.(value & flag & info ["arg"] ~doc)
    in
    let debug =
        let doc = "Enable debug" in
        Cmdliner.Arg.(value & flag & info ["d"; "debug"] ~doc)
    in
    let shell =
        let doc = "Invoke commandin a shell" in
        Cmdliner.Arg.(value & opt (some string) None & info ["c"; "shell"] ~docv:"CMD" ~doc)
    in
    let stats =
        let doc = "Print statistics about the file" in
        Cmdliner.Arg.(value & flag & info ["s"; "stats"] ~doc)
    in
    let doc = "Call the command on every result in file, piping the result's output into the command's input" in
    Cmdliner.Term.(pure frogiter $ file $ cmd $ prelude $ arg $ debug $ shell $ stats),
    Cmdliner.Term.info "iter" ~doc


