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

let main_config_file = "/etc/froglimit.conf"

let main _ = ()

let config_term =
  let open Cmdliner in
  let aux l =
    FrogConfig.parse_files l (FrogConfig.parse_or_empty main_config_file)
  in
  let config_file =
    let doc = "Path to configuration file for froglimit" in
    Arg.(value & opt_all non_dir_file [] & info ["c"; "config"] ~doc)
  in
  Term.(pure aux $ config_file)

let term =
  let open Cmdliner in
  let mem_limit =
    let doc = "Memory limit" in
    Arg.(value & opt (some int) None & info ["m"; "memory"] ~doc)
  in
  let doc = "Executes given command with limits" in
  let man = [] in
  Term.(pure main $ mem_limit),
  Term.info ~man ~doc "froglimit"

let () =
  match Cmdliner.Term.eval_choice term [] with
  | `Version | `Help | `Error `Parse | `Error `Term | `Error `Exn -> exit 2
  | `Ok () -> ()

