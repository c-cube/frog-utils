
(* This file is free software, part of frog-utils. See file "license" for more details. *)

module Res = FrogRes
module Conf = FrogConfig
module Prover = FrogProver

(*
module TPTP = struct
  let get_str_ d x =
    try Some (Conf.get_string d x)
    with Not_found -> None

  let make_command ?tptp p ~timeout ~memory ~file =
    let env = match tptp with
      | None -> [||]
      | Some f -> [| "TPTP", f |]
    in
    make_command ~env p ~timeout ~memory ~file

  let run_cmd ?tptp ~timeout ~memory ~config ~prover ~file () =
    let env = match tptp with
      | Some f -> Some [| "TPTP", f |]
      | None ->
        match get_str_ config "TPTP" with
        | None -> None
        | Some f -> Some  [| "TPTP", f |]
    in
    let prover = Prover.find_config config prover in
    run_cmd ?env ~timeout ~memory ~prover ~file ()

end

   *)
