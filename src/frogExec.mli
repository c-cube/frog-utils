(*
copyright (c) 2013-2015, guillaume bury
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

type handle = {
  start : float;
  main_pid : int;
  memory_cgroup : Cgroups.Hierarchy.cgroup;
  cpuacct_cgroup : Cgroups.Hierarchy.cgroup;
}

val spawn : f:(handle -> 'a) -> string -> (unit -> 'b) -> 'a

val mk_limits : ?mem_limit:int -> ?time_limit:float -> ?cpu_limit:float -> handle -> unit

class virtual ['a, 'b] limit :
  'a -> handle ->
  object
    val handle : handle
    val mutable value : 'a
    method private virtual disable : unit
    method private virtual enable : unit
    method set : 'a -> unit
    method virtual stats : 'b
    method value : 'a
  end

class memory_limit :
  handle ->
  object
    val handle : handle
    val mutable value : int
    method private disable : unit
    method private enable : unit
    method set : int -> unit
    method stats : unit
    method value : int
  end

class real_time_limit :
  handle ->
  object
    val mutable alarm : (Lwt_engine.event * Lwt_engine.event) option
    val handle : handle
    val mutable value : float
    method disable : unit
    method enable : unit
    method set : float -> unit
    method stats : float
    method value : float
  end

class cpu_time_limit :
  handle ->
  object
    val mutable event : Lwt_engine.event option
    val handle : handle
    val mutable value : float
    method disable : unit
    method enable : unit
    method set : float -> unit
    method stats : unit
    method value : float
  end

