open OUnit2
open Chess.State
open Values
open Helper

let turn_test name expected_output state =
  name >:: fun _ -> assert_equal expected_output (turn state) ~printer:string_of_bool

let result_test name expected_output state =
  name >:: fun _ -> assert_equal expected_output (result state) ~printer:result_printer

let checkmate_test name state expected_output =
  name >:: fun _ -> checkmate state |> assert_equal expected_output ~printer:string_of_bool

let turn_tests =
  [
    turn_test "turn of initial state is false (white)" false initial_state;
    turn_test "turn of state after one move is true (black)" true fst_state;
    turn_test "turn of state after 2 moves is false (white)" false snd_state;
  ]

let result_tests =
  [
    result_test "result of initial state is Playing" Playing initial_state;
    (* result_test "result of scholar's checkmate is WhiteWin" WhiteWin scholar_state; *)
  ]

let checkmate_tests =
  [
    checkmate_test "Initial board is not a checkmate state" initial_state false;
    checkmate_test "Scholar's checkmate is in a checkmate state" scholar_state true;
    checkmate_test "Double check is not a checkmate state" double_state false;
  ]

let suite =
  "test suite for State" >::: List.flatten [ turn_tests; result_tests; checkmate_tests ]

let _ = run_test_tt_main suite