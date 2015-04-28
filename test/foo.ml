
open OUnit2

let id x = x

(* Testing for froglock *)
let expect_line stream l =
  let s = String.init (String.length l) (fun _ -> Stream.next stream) in
  assert_equal ~printer:id l s;
  assert_equal '\n' (Stream.next stream)

let expect lines s = List.iter (expect_line s) lines

let lock_echo_1 ctxt =
  assert_command
    ~foutput:(expect ["1"])
    ~ctxt "./froglock.native" ["--"; "echo"; "1"]

(* Testing for frogmap *)
let cmp_time diff a b = abs_float (b -. a) <= diff

let check_times e res =
  let open FrogMapState in
  Lwt.return (e +. (res.res_time -. float_of_string res.res_arg))

let map_sleep_time times ctxt =
  let file, ch = bracket_tmpfile ~prefix:"frog" ~suffix:".json" ctxt in
  close_out ch;
  assert_command ~ctxt "./frogmap.native"
    (["-p"; "false"; "-o"; file; "sleep"] @ (List.map string_of_int times));
  let diff = Lwt_main.run (FrogMapState.fold_state_s check_times (fun _ -> Lwt.return 0.) file) in
  let max_diff = 0.01 *. (float) (List.length times) in
  assert_equal ~ctxt ~cmp:(cmp_time max_diff) ~printer:string_of_float 0. diff

(* All tests *)

let all_tests =
  "all" >::: [
    "lock_echo_1">:: lock_echo_1;
    "map_sleep_time_1" >:: map_sleep_time [1];
    "map_sleep_time_3" >:: map_sleep_time [1;2;3];
    "map_sleep_time_3" >:: map_sleep_time [1;1;1;1;1;1;1;1;1;1];
  ]
;;

let () =
  run_test_tt_main all_tests


