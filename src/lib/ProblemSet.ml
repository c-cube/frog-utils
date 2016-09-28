
(* This file is free software, part of frog-utils. See file "license" for more details. *)

type t = Problem.t list

let make ~default_expect l =
  let pool = Lwt_pool.create 30 (fun () -> Lwt.return_unit) in
  let%lwt l =
    Lwt_list.map_p
      (fun file ->
         Lwt_pool.use pool
           (fun () -> Problem.make ~default_expect ~file ()))
      l
  in
  let l = Misc.Err.seq_list l in
  (* sort by alphabetic order *)
  let l = Misc.Err.(l >|= List.sort Problem.compare_name) in
  Lwt.return l

let size = List.length

let of_dir ~default_expect ?filter:(p=fun _ -> true) d =
  let l = Misc.File.walk d in
  let l = Misc.List.filter_map
      (fun (kind,f) -> match kind with
         | `File when p f -> Some f
         | _ -> None
      ) l
  in
  make ~default_expect l

let print out set =
  Format.fprintf out "@[<hv>%a@]" (Format.pp_print_list Problem.print) set

