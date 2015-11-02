
(* This file is free software, part of frog-utils. See file "license" for more details. *)

module Opt = struct
  let (>>=) o f = match o with
    | None -> None
    | Some x -> f x

  let (>|=) o f = match o with
    | None -> None
    | Some x -> Some (f x)

  let get default x = match x with
    | None -> default
    | Some y -> y
end

module File = struct

  let with_in ~file f = Lwt_io.with_file ~mode:Lwt_io.input file f

  let with_out ~file f = Lwt_io.with_file ~mode:Lwt_io.output file f

  let read_all ic = Lwt_io.read ic

  (* traverse directory *)
  let walk d =
    let rec aux acc d =
      if Sys.is_directory d
      then
        let entries = Sys.readdir d in
        let acc = (`Dir, d) :: acc in
        Array.fold_left
          (fun acc s ->
            aux acc (Filename.concat d s)
          ) acc entries
      else (`File, d) :: acc
    in
    aux [] d

end


