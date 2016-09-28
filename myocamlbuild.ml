
(* OASIS_START *)
(* OASIS_STOP *)

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    dispatch_default hook;
    Ocamlbuild_js_of_ocaml.dispatcher
      ~oasis_executables:["src/js/frogwebclient.byte"] hook
  )
