
(* This file is free software, part of frog-utils. See file "license" for more details. *)

module D : sig
  type t = Sqlite3.Data.t =
    | NONE
    | NULL
    | INT of int64
    | FLOAT of float
    | TEXT of string
    | BLOB of string

  val int : int -> t
  val string : string -> t

  val as_int : t -> int
  val as_string : t -> string
end

type data = D.t

type row = data array

type rc = Sqlite3.Rc.t (** Error code *)

type stmt = string (** A query *)

type t = Sqlite3.db
(** A connection to some Sqlite DB *)

module Cursor : sig
  type t

  val close : t -> unit
  (** Finalize the cursor, that is, cleans up resource *)

  val next : t -> row option
  (** [next t] consumes one row and saves it in the cursor *)

  val junk : t -> unit
  (** same as [ignore (next t)] *)

  val head : t -> row option
  (** Current row *)

  val head_exn : t -> row
  (** Current row
      @raise Failure if there is no current row *)

  val iter : f:(row -> unit) -> t -> unit

  val to_list_rev : t -> row list
  (** list of rows, in reverse order *)

  val to_list : t -> row list
end

val check_ret : rc -> unit
(** Fail if the return code is not DONE or OK *)

val open_ : string -> t

val with_open : string -> (t -> 'a) -> 'a

val exec : t -> stmt -> (Cursor.t -> 'a) -> 'a
(** Execute the query and return a cursor of results *)

val exec_a : t -> stmt -> data array -> (Cursor.t -> 'a) -> 'a
(** [exec_a db stmt params f] executes the parametrized query [stmt],
    gives the cursor to [f], then closes it *)

val exec1 : t -> stmt -> data -> (Cursor.t -> 'a) -> 'a
(** Same as {!exec_a} but with only one parameter *)

