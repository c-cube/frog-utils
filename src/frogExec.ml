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

let () = Random.self_init ()

type 'a limit = {
  value : 'a;
  enforce : handle -> unit;
  disable : handle -> unit;
}

and handle = {
  main_pid : int;
  memory_cgroup : Cgroups.Hierarchy.cgroup;
  cpuacct_cgroup : Cgroups.Hierarchy.cgroup;

  (* Ressource limits *)
  mutable mem_limit : int limit;
  mutable cpu_limit : float limit;
  mutable time_limit : float limit;
}

(* Cgroup management *)
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
    Cgroups.Hierarchy.mk_sub parent s 0o777

let find_root subsystem name =
  Cgroups.Hierarchy.find_exn (Format.sprintf "%s:%s" subsystem name)

let dummy_limit value = { value; enforce = (fun _ -> ()); disable = (fun _ -> ()); }

let make_handle
    ?(mem_limit = dummy_limit 0)
    ?(cpu_limit = dummy_limit 0.)
    ?(time_limit = dummy_limit 0.)
    root main_pid =
  let memory_cgroup = new_cgroup (find_root "memory" root) in
  let cpuacct_cgroup = new_cgroup (find_root "cpuacct" root) in
  Cgroups.Hierarchy.add_process memory_cgroup main_pid;
  Cgroups.Hierarchy.add_process cpuacct_cgroup main_pid;
  { main_pid; memory_cgroup; cpuacct_cgroup;
    mem_limit; cpu_limit; time_limit; }

(* Memory management *)
let mem_limit limit =
  let open Cgroups.Subsystem in {
  value = limit;
  enforce = (fun handle -> Param.set Memory.limit_in_bytes handle.memory_cgroup limit);
  disable = (fun handle -> Param.set Memory.limit_in_bytes handle.memory_cgroup 0);
}

(* Real time management *)
let real_time_limit limit =
  let soft_alarm, hard_alarm = ref None, ref None in
  {
  value = limit;
  enforce = (fun handle ->
      soft_alarm := Some (Lwt_engine.on_timer limit false (fun _ ->
          Unix.kill handle.main_pid Sys.sigalrm));
      hard_alarm := Some (Lwt_engine.on_timer (limit +. 1.) false (fun _ ->
          Unix.kill handle.main_pid Sys.sigkill)));
  disable = (fun _ ->
      Opt.iter Lwt_engine.stop_event !soft_alarm;
      Opt.iter Lwt_engine.stop_event !hard_alarm);
  }
(*
(* CPU time management *)
let limit_cpu_time cgroup_name limit pid =
  let parent = Cgroups.Hierarchy.find_exn (
      Format.asprintf "cpuacct:%s" cgroup_name) in
  let g = new_cgroup parent in
  Cgroups.Hierarchy.add_process g pid;
  let n = List.length (Cgroups.Subsystem.Param.get Cgroups.Subsystem.Cpuacct.usage_percpu g) in
  let max_speed = float_of_int n +. 1. in
  let rec aux _ =
    if List.mem pid (Cgroups.Hierarchy.processes g) then begin
      let t = Cgroups.Subsystem.Param.get Cgroups.Subsystem.Cpuacct.usage g in
      let t_s = float_of_int t /. 1_000_000_000. in
      if t_s > limit +. 1. then
        Unix.kill pid Sys.sigkill
      else begin
        if t_s > limit then (* Unix.kill pid Sys.sigxcpu *) ();
        let delay = (limit -. t_s) /. max_speed in
        let wait = max 0.1 delay in
        ignore (Lwt_engine.on_timer wait false aux)
      end
    end
  in
  aux Lwt_engine.fake_event

(* Spawning with limits *)
let spawn limiters f =
  match Unix.fork () with
  | 0 ->
    Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> ignore (f ())));
    Unix.sleep 1; f ()
  | pid ->
    apply limiters pid;
    Unix.kill pid Sys.sigusr1;
    let _, res = Unix.waitpid [] pid in
    res

let execv ?(limiters=[]) cmd args =
  spawn limiters (fun () -> Unix.execv cmd args)

   *)
