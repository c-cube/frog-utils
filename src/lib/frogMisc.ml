
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

module Err = struct
  type 'a t = [`Ok of 'a | `Error of string]

  let return x = `Ok x
  let fail e = `Error e

  let (>>=) e f = match e with
    | `Error e -> `Error e
    | `Ok x -> f x

  let (>|=) e f = match e with
    | `Error e -> `Error e
    | `Ok x -> `Ok (f x)

  (* 'a or_error list -> 'a list or_error *)
  let seq_list l =
    let rec aux acc = function
      | [] -> `Ok (List.rev acc)
      | `Error e :: _ -> `Error e
      | `Ok x :: l' -> aux (x :: acc) l'
    in
    aux [] l
end

module LwtErr = struct
  type 'a t = 'a Err.t Lwt.t

  let return x = Lwt.return (`Ok x)
  let fail e = Lwt.return (`Error e)

  let lift : 'a Err.t -> 'a t = Lwt.return
  let ok : 'a Lwt.t -> 'a t = fun x -> Lwt.map (fun y -> `Ok y) x

  let (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  = fun e f ->
    Lwt.bind e (function
      | `Error e -> Lwt.return (`Error e)
      | `Ok x -> f x
    )

  let (>|=) : 'a t -> ('a -> 'b) -> 'b t
  = fun e f ->
    Lwt.map (function
      | `Error e -> `Error e
      | `Ok x -> `Ok (f x)
    ) e
end

module List = struct
  let filter_map f l =
    let rec recurse acc l = match l with
    | [] -> List.rev acc
    | x::l' ->
      let acc' = match f x with | None -> acc | Some y -> y::acc in
      recurse acc' l'
    in recurse [] l
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

(** Yay formatting! *)
module Fmt = struct
  let fpf = Format.fprintf

  type color =
    [ `Red
    | `Yellow
    | `Green
    | `Blue
    ]

  let int_of_color_ = function
    | `Red -> 1
    | `Green -> 2
    | `Yellow -> 3
    | `Blue -> 4

  (* same as [pp], but in color [c] *)
  let in_color c pp out x =
    let n = int_of_color_ c in
    fpf out "\x1b[3%dm" n;
    pp out x;
    fpf out "\x1b[0m"

  (* same as [pp], but in bold color [c] *)
  let in_bold_color c pp out x =
    let n = int_of_color_ c in
    fpf out "\x1b[3%d;1m" n;
    pp out x;
    fpf out "\x1b[0m"
end

