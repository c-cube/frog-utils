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

(* Pid/cgroup infos *)
type handle = {
  start : float;
  main_pid : int;
  memory_cgroup : Cgroups.Hierarchy.cgroup;
  cpuacct_cgroup : Cgroups.Hierarchy.cgroup;
}

(* Limits *)
class virtual ['a, 'b] limit value handle = object(self)

  val handle : handle = handle
  val mutable value : 'a = value

  method value = value
  method virtual stats : 'b

  method virtual private enable : unit
  method virtual private disable : unit

  method set v =
    self#disable;
    value <- v;
    self#enable

end

(* Memory management *)
class memory_limit info = object
  inherit [int, unit] limit 0 info as super

  method private stats = ()

  method private enable = ()
  method private disable = ()

  method! set v =
    super#set v;
    let open Cgroups.Subsystem in
    Param.set Memory.limit_in_bytes info.memory_cgroup v

end

(* Real time management *)
class real_time_limit info = object(self)
  inherit [float, float] limit 0. info

  val mutable alarm = None

  method stats = Unix.gettimeofday () -. info.start

  method disable =
    match alarm with
    | Some (soft, hard) ->
      Lwt_engine.stop_event soft;
      Lwt_engine.stop_event hard;
      alarm <- None
    | None -> ()

  method enable =
    let time = value -. self#stats in
    alarm <- Some (
        (Lwt_engine.on_timer time false (fun _ ->
             Unix.kill info.main_pid Sys.sigalrm)),
        (Lwt_engine.on_timer (time +. 1.) false (fun _ ->
             Unix.kill info.main_pid Sys.sigkill))
      )

end

(* CPU time management *)
class cpu_time_limit info = object(self)
  inherit [float, unit] limit 0. info

  val mutable event = None

  method stats = ()

  method enable =
    let n = List.length (Cgroups.Subsystem.Param.get Cgroups.Subsystem.Cpuacct.usage_percpu info.cpuacct_cgroup) in
    let max_speed = float_of_int n +. 1. in
    let rec aux _ =
      if List.mem info.main_pid (Cgroups.Hierarchy.processes info.cpuacct_cgroup) then begin
        let limit = self#value in
        let t = Cgroups.Subsystem.Param.get Cgroups.Subsystem.Cpuacct.usage info.cpuacct_cgroup in
        let t_s = float_of_int t /. 1_000_000_000. in
        if t_s > limit +. 1. then
          Unix.kill info.main_pid Sys.sigkill
        else begin
          if t_s > limit then (* Unix.kill pid Sys.sigxcpu *) ();
          let delay = (limit -. t_s) /. max_speed in
          let wait = max 0.1 delay in
          event <- Some (Lwt_engine.on_timer wait false aux)
        end
      end
    in
    aux Lwt_engine.fake_event

  method disable =
    match event with
    | Some ev -> Lwt_engine.stop_event ev; event <- None
    | None -> ()

end

(* Cgroup management *)
let () = Random.self_init ()

let random_name ?(length=7) base =
  let s = String.init length (fun _ ->
      char_of_int (int_of_char '0' + Random.int 10)) in
  base ^ s

let rec new_cgroup parent =
  let s = random_name "frogexec_" in
  let l = Cgroups.Hierarchy.children parent in
  if List.exists (fun g -> s = Cgroups.Hierarchy.name g) l then
    new_cgroup parent
  else
    Cgroups.Hierarchy.make_sub ~perm:0o770 parent s

let find_root subsystem name =
  Cgroups.Hierarchy.find_exn (Format.sprintf "%s:%s" subsystem name)

(* Handle creation *)
let make_handle root main_pid =
  let memory_cgroup = new_cgroup (find_root "memory" root) in
  let cpuacct_cgroup = new_cgroup (find_root "cpuacct" root) in
  Cgroups.Hierarchy.add_process memory_cgroup main_pid;
  Cgroups.Hierarchy.add_process cpuacct_cgroup main_pid;
  let start = Unix.gettimeofday () in
  { start; main_pid; memory_cgroup; cpuacct_cgroup; }

(* Spawning with limits *)
let spawn ~f root todo =
  match Unix.fork () with
  | 0 ->
    begin try
        Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ ->
            ignore (todo ());
            exit 0
          ));
        Unix.sleep 1;
        assert false
      with _ -> exit 0
    end
  | pid ->
    let h = make_handle root pid in
    let res = f h in
    Unix.kill pid Sys.sigusr1;
    res

let mk_limits
    ?mem_limit
    ?time_limit
    ?cpu_limit
    handle =
  let iter f = function Some a -> f a | None -> () in
  let _ =
    let m = new memory_limit handle in
    iter m#set mem_limit in
  let _ =
    let m = new real_time_limit handle in
    iter m#set time_limit in
  let _ =
    let m = new cpu_time_limit handle in
    iter m#set cpu_limit in
  ()
