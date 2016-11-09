
(* This file is free software, part of frog-utils. See file "license" for more details. *)

(** {1 Run Prover}

    Utils to run a theorem prover (or a similar tool) and extract its result
*)

(** {2 Prover configurations} *)

type version =
  | Tag of string
  | Git of string * string  (* branch & commit hash *)
  [@@deriving yojson]

type t = {
  (* Prover identification *)
  name : string;
  version : version;

  (* Pover execution *)
  binary: string;       (* name of the program itself *)
  cmd: string;          (* the command line to run.
                           possibly contains $binary, $file, $memory and $timeout *)

  (* Result analysis *)
  unsat   : string option;  (* regex for "unsat" *)
  sat     : string option;  (* regex for "sat" *)
  unknown : string option;  (* regex for "unknown" *)
  timeout : string option;  (* regex for "timeout" *)
  memory  : string option;  (* regex for "out of memory" *)
} [@@deriving yojson]
(** The type of provers configurations *)

val name : t -> string
(** Prover name *)

val pp_name : Format.formatter -> t -> unit

val equal : t -> t -> bool
(** Equality (by name) *)

val make_command :
  ?env:(string * string) array ->
  t ->
  timeout:int ->
  memory:int ->
  file:string ->
  string

(** Map by name *)
module Map_name : sig
  include Map.S with type key = t
  val to_list : 'a t -> (key * 'a) list
  val of_list : (key * 'a) list -> 'a t
end

(** Map with full compare *)
module Map : sig
  include Map.S with type key = t
  val to_list : 'a t -> (key * 'a) list
  val of_list : (key * 'a) list -> 'a t
end

module Set : Set.S with type elt = t

val to_html_name : t -> Html.t
val to_html_fullname : t -> Html.t
val to_html_full : t -> Html.t

