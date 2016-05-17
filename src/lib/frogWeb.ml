
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 HTML Interface} *)

module Html = Cow.Html

type html = Html.t

type uri = Uri.t

module HMap = Opium_hmap

(** {2 Encoding Records in HTML} *)

module Record : sig
  type t
  val start : t
  val add : string -> html -> t -> t
  val add_int : string -> int -> t -> t
  val add_string : string -> string -> t -> t
  val add_string_option : string -> string option -> t -> t
  val add_bool : string -> bool -> t -> t
  val close : t -> html
end = struct
  type t = (string * html) list
  let start = []
  let add s f l = (s, f) :: l
  let add_with_ fun_ s f l = add s (fun_ f) l
  let add_int = add_with_ Html.int
  let add_string = add_with_ Html.string
  let add_string_option = add_with_ (function None -> Html.empty | Some s -> Html.string s)
  let add_bool = add_with_ (fun b -> Html.string (string_of_bool b))
  let close l =
    Html.Create.table ~flags:[Html.Create.Tags.Headings_fst_col]
      ~row:(fun (s,f) -> [Html.string s; f])
      (List.rev l)
end


(* TODO: global router? as a Opium_hmap for instance? *)
