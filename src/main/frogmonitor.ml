
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Monitor Events} *)

open Frog
open Frog_server

module M = IPC_message
module E = Misc.LwtErr
module Fmt = CCFormat

let on_message msg: unit =
  Lwt_log.ign_debug_f "(read message: %s)\n%!" (IPC_message.show msg);
  begin match msg with
    | M.Start_bench n ->
      Format.printf "@{<Green>start benchmark (%d items)@}@." n;
    | M.Finish_bench ->
      Format.printf "@{<Green>finish benchmark@}@.";
    | M.Event (Event.Prover_run res) ->
      Test_run.print_result res
    | M.Event (Event.Checker_run _ as e) ->
      Format.printf "> %a@." Event.pp e
    | _ -> ()
  end


let main port debug =
  let open E.Infix in
  Fmt.set_color_default true;
  if debug then Lwt_log.add_rule "*" Lwt_log.Debug;
  IPC_client.connect_or_spawn port
    (fun c ->
       Lwt_log.ign_debug_f "opened connection to daemon";
       (* print every message *)
       IPC_client.on_msg c
         (fun msg -> on_message msg; `Continue);
       IPC_client.wait_close c)
  |> Lwt_main.run

let term =
  let open Cmdliner in
  let port =
    Arg.(value & opt int IPC_daemon.default_port & info ["p"; "port"] ~doc:"port to listen on")
  and debug  =
    Arg.(value & flag & info ["d"; "debug"] ~doc:"activate debug")
  in
  let doc = "monitor events" in
  let man = [
    `S "SYNOPSIS";
    `I ("$(b,frogmonitor [OPTIONS])", "monitor daemon");
  ] in
  Term.(pure main $ port $ debug),
  Term.info ~man ~doc "frogmonitor"

let () =
  match Cmdliner.Term.eval term with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok () -> ()
