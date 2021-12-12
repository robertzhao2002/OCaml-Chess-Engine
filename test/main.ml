(* Main test suite where we can add our testing explanation? *)
open OUnit2

let main_suite =
  "test suite for Chess"
  >::: List.flatten
         [
           Command_test.tests;
           Piece_test.tests;
           Board_test.tests;
           State_test.tests;
         ]

let _ = run_test_tt_main main_suite
