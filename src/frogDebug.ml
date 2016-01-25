
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Debug function} *)

let debug_ = ref false

let set_debug b = debug_ := b
let enable_debug () = debug_ := true

let debug fmt =
  if !debug_
  then Format.kfprintf
    (fun _ -> Format.fprintf Format.std_formatter "@.")
    Format.std_formatter fmt
  else Format.ifprintf Format.std_formatter fmt

