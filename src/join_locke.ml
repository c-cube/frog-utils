(*
copyright (c) 2013-2014, simon cruanes
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

(** {1 Scheduling script} *)

(* spawn a daemon *)
let spawn_daemon

(* the file is divided into two parts:
    - header (lock length 0), an integer followed by \n
    - body: a queue of 

(* main task: acquire lock file, execute command [cmd], release lock *)
let main lock_file cmd =
  with_locked_file lock_file
    (fun 


(** {2 Main} *)

let lock_file_ = ref "/tmp/join-locke"
let cmd_ = ref []
let push_cmd_ s = cmd_ := s :: !cmd_

let options =
  [ "-lock", Arg.Set_string lock_file_, "lock file to use"
  ]
(* TODO: option to send a mail when the job finishes *)
(* TODO: option to specify estimated completion time *)

let () =
  Arg.parse options push_cmd "locke [options] <cmd> <args>";
  let cmd = List.rev !cmd_ in
  main !lock_file_ cmd
