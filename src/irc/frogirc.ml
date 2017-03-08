
module C = Calculon

(* TODO: cmdliner for parsing this *)
let config = {
  C.Config.default with C.Config.channel = "#deducteam"; nick = "froggy"; }

let main () =
  let plugins =
    [ Frog_irc.plugin;
      C.Plugin_factoids.plugin;
      C.Plugin_history.plugin ();
    ] in
  C.Run_main.main config plugins


let () = main () |> Lwt_main.run
