
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Simple wrapper for HTML} *)

include Tyxml.Html

type t = Html_types.div_content_fun elt
type html = t

(** {2 Encoding Records in HTML} *)

let to_string h =
  Misc.Fmt.to_string (pp_elt()) h

module Record = struct
  type t = (string * html) list
  let start = []
  let add s f l = (s, f) :: l
  let add_with_ fun_ ?(raw=false) s f l =
    let body = txt (fun_ f) in
    add s (div [if raw then pre [body] else body]) l
  let add_int = add_with_ string_of_int
  let add_float = add_with_ string_of_float
  let add_string = add_with_ (fun x->x)
  let add_string_option = add_with_ (function None -> "" | Some s -> s)
  let add_bool = add_with_ string_of_bool
  let add_record = (@)
  let close l =
    table
      (List.rev_map
         (fun (s,f) -> tr [td [txt s]; td [f]])
         l)
end

(* TODO: same as record, but for full tables? *)
