
open OUnit2

let id x = x

let expect_line stream l =
  let s = String.init (String.length l) (fun _ -> Stream.next stream) in
  assert_equal ~printer:id l s;
  assert_equal '\n' (Stream.next stream)

let expect lines s = List.iter (expect_line s) lines

let lock_echo_1 ctxt =
  assert_command
    ~foutput:(expect ["1"])
    ~ctxt "./froglock.native" ["--"; "echo"; "1"]

let all_tests =
  "all">::: [
    "lock_echo_1">:: lock_echo_1;
  ]
;;

let () =
  run_test_tt_main all_tests


