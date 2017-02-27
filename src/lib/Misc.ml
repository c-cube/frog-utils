
(* This file is free software, part of frog-utils. See file "license" for more details. *)

open Result

module Opt = struct
  let return x = Some x
  let none = None
  let (>>=) o f = match o with
    | None -> None
    | Some x -> f x

  let (>|=) o f = match o with
    | None -> None
    | Some x -> Some (f x)

  let iter ~f = function
    | None -> ()
    | Some x -> f x

  let get default x = match x with
    | None -> default
    | Some y -> y
end

module Str = struct
  let split ~by s =
    let i = String.index s by in
    String.sub s 0 i, String.sub s (i+1) (String.length s -i-1)
end

(** Yay formatting! *)
module Fmt = struct
  let fpf = Format.fprintf
  let to_string f x = Format.asprintf "%a%!" f x

  let ksprintf ~f fmt =
    let buf = Buffer.create 32 in
    let out = Format.formatter_of_buffer buf in
    Format.kfprintf
      (fun _ -> Format.pp_print_flush out (); f (Buffer.contents buf))
      out fmt

  type color =
    [ `Red
    | `Yellow
    | `Green
    | `Blue
    | `Normal
    ]

  let int_of_color_ = function
    | `Normal -> 0
    | `Red -> 1
    | `Green -> 2
    | `Yellow -> 3
    | `Blue -> 4

  let pp_list ?(start="") ?(stop="") ?(sep=",") pp fmt l =
    let rec pp_list l = match l with
      | x::((_::_) as l) ->
        pp fmt x;
        Format.pp_print_string fmt sep;
        Format.pp_print_cut fmt ();
        pp_list l
      | x::[] -> pp fmt x
      | [] -> ()
    in
    Format.pp_print_string fmt start;
    pp_list l;
    Format.pp_print_string fmt stop

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

module Err = struct
  type 'a t = ('a, string) result

  let return x = Ok x
  let fail e = Error e

  let (>>=) e f = match e with
    | Error e -> Error e
    | Ok x -> f x

  let (>|=) e f = match e with
    | Error e -> Error e
    | Ok x -> Ok (f x)

  let of_exn (x:(_,exn) result) : _ t = match x with
    | Ok x -> Ok x
    | Error e -> Error (Printexc.to_string e)

  let to_exn (x:_ t) : (_,exn) result = match x with
    | Ok x -> Ok x
    | Error e -> Error (Failure e)

  let (<*>) f x = match f, x with
    | Ok f, Ok x -> Ok (f x)
    | Error e, _ -> Error e
    | _, Error e -> Error e

  (* 'a or_error list -> 'a list or_error *)
  let seq_list l =
    let rec aux acc = function
      | [] -> Ok (List.rev acc)
      | Error e :: _ -> Error e
      | Ok x :: l' -> aux (x :: acc) l'
    in
    aux [] l
end

module LwtErr = struct
  type 'a t = 'a Err.t Lwt.t

  let return x = Lwt.return (Ok x)
  let fail e = Lwt.return (Error e)
  let failf e = Fmt.ksprintf ~f:fail e

  let lift : 'a Err.t -> 'a t = Lwt.return
  let ok : 'a Lwt.t -> 'a t = fun x -> Lwt.map (fun y -> Ok y) x

  let add_ctx : string -> 'a t -> 'a t = fun msg x ->
    Lwt.map
      (function Error e -> Error (e ^ "\ncontext:" ^ msg) | Ok x -> Ok x)
      x

  let add_ctxf msg =
    Fmt.ksprintf ~f:(fun msg -> add_ctx msg) msg

  let to_exn : 'a t -> 'a Lwt.t =
    fun x->
      Lwt.bind x
        (function Error e -> Lwt.fail (Failure e) | Ok x -> Lwt.return x)

  module Infix = struct
    let (>>=) : 'a t -> ('a -> 'b t) -> 'b t
      = fun e f ->
        Lwt.bind e (function
          | Error e -> Lwt.return (Error e)
          | Ok x -> f x
        )

    let (>>?=) : 'a t -> ('a -> 'b Err.t) -> 'b t
      = fun e f ->
        Lwt.bind e (function
          | Error e -> Lwt.return (Error e)
          | Ok x -> f x |> Lwt.return
        )

    let (>|=) : 'a t -> ('a -> 'b) -> 'b t
      = fun e f ->
        Lwt.map (function
          | Error e -> Error e
          | Ok x -> Ok (f x)
        ) e
  end
  include Infix

  let of_exn : ('a, exn) result Lwt.t -> 'a t = fun x -> Lwt.map Err.of_exn x

  let rec map_s : ('a -> 'b t) -> 'a list -> 'b list t
    = fun f l -> match l with
    | [] -> return []
    | x :: tail ->
      f x >>= fun x' -> map_s f tail >|= fun tail' -> x' :: tail'

  let map_p : ('a -> 'b t) -> 'a list -> 'b list t
    = fun f l ->
      let%lwt l = Lwt_list.map_p f l in
      Lwt.return (Err.seq_list l)
end

module List = struct
  include List
  let return x = [x]
  let filter_map f l =
    let rec recurse acc l = match l with
    | [] -> List.rev acc
    | x::l' ->
      let acc' = match f x with | None -> acc | Some y -> y::acc in
      recurse acc' l'
    in recurse [] l
  let flat_map f l = flatten (map f l)
  let cons_opt o l = match o with
    | None -> l
    | Some x -> x :: l
end

module Map_(O:Map.OrderedType) = struct
  include Map.Make(O)
  let to_list m = fold (fun prover res acc -> (prover,res)::acc) m []
  let of_list l = List.fold_left (fun acc (p,r) -> add p r acc) empty l
end
module Set_(O:Map.OrderedType) = struct
  include Set.Make(O)
  let to_list = elements
  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module StrMap = Map_(String)
module StrSet = Set_(String)
