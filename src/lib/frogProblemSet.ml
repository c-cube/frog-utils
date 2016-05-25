
(* This file is free software, part of frog-utils. See file "license" for more details. *)

module Problem = FrogProblem

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
  Format.fprintf out "@[<hv>%a@]" (Format.pp_print_list Problem.print) set

let maki = Maki.Value.set Problem.maki

let to_html uri_of_pb l =
  let module H = FrogWeb.Html in
  let f pb = H.a ~href:(uri_of_pb pb) (Problem.to_html_name pb) in
  H.div ~attrs:["class", "problem_set"]
    (H.list (List.map f l))
