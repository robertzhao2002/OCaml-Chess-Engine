open OUnit2
open Chess.Command
open Values

let parse_test (name : string) (str : string) (expected_output : command) : test
    =
  name >:: fun _ -> assert_equal expected_output (parse str)

let parse_excep_test (name : string) (str : string) (e : exn) : test =
  name >:: fun _ -> assert_raises e (fun () -> parse str)

let promotion_parse_test
    (name : string)
    (str : string)
    (expected_output : promotion_pieces) : test =
  name >:: fun _ -> assert_equal expected_output (promotion_parse str)

let parse_tests =
  [
    parse_test "Input reset shoud parse to Reset" "reset" Reset;
    parse_test "Input rEsEt shoud parse to Reset" "rEsEt" Reset;
    parse_test "Input \"         rEsEt         \" shoud parse to Reset"
      (empty_space ^ "rEsEt" ^ empty_space)
      Reset;
    parse_test "Input Quit should parse to Quit" "Quit" Quit;
    parse_test "Input  quIT  should parse to Quit" "  quIT  " Quit;
    parse_test "Input dRaW should parse to Draw" "dRaW" Draw;
    parse_test "Input Draw should parse to Draw" "Draw" Draw;
    parse_test "Input scOre should parse to Draw" "scOre" Score;
    parse_test "Input Score should parse to Draw" "Score" Score;
    parse_test "Input a3 c6 should parse to Move (5,0) (2,2)" "a3 c6"
      (Move ((5, 0), (2, 2)));
    parse_test "Input A3 C6 should parse to Move (5,0) (2,2)" "A3 C6"
      (Move ((5, 0), (2, 2)));
    parse_test "Input a1 h7 should parse to Move (7,0) (1,7)" "a1 h7"
      (Move ((7, 0), (1, 7)));
    parse_test "input \"a3         c6\" should parse to Move (5,0) (2,2)"
      ("a3" ^ empty_space ^ "c6")
      (Move ((5, 0), (2, 2)));
    parse_test
      "input \"         a3         c6         \" should parse to Move (5,0) \
       (2,2)"
      (empty_space ^ "a3" ^ empty_space ^ "c6" ^ empty_space)
      (Move ((5, 0), (2, 2)));
    parse_test "Input \"undo\" shoud parse to Undo" "undo" Undo;
    parse_test "Input \"         unDO         \" shoud parse to Undo"
      (empty_space ^ "unDO" ^ empty_space)
      Undo;
    parse_test "Input \"UNDO         \" shoud parse to Undo"
      ("UNDO" ^ empty_space) Undo;
    parse_test "Input \"HELP\" shoud parse to Help" "HELP" Help;
    parse_test "Input \"HeLP         \" shoud parse to Help"
      ("HeLP" ^ empty_space) Help;
    parse_test "Input \"         help         \" shoud parse to Help"
      (empty_space ^ "help" ^ empty_space)
      Help;
    parse_test "Input \"Rules\" should parse to Rules" "Rules" Rules;
    parse_test "Input \"         ruLES         \" shoud parse to Help"
      (empty_space ^ "ruLES" ^ empty_space)
      Rules;
  ]

let parse_excep_tests =
  [
    parse_excep_test "An empty input should raise Malformed" "" Malformed;
    parse_excep_test "An empty input should raise Malformed" "    " Malformed;
    parse_excep_test "An input of yellow should raise Malformed" "yellow"
      Malformed;
    parse_excep_test "An input of quite should raise Malformed" "quite"
      Malformed;
    parse_excep_test "An input of a5 c10 should raise Malformed" "a5 c10"
      Malformed;
    parse_excep_test "An input of a7 should raise Malformed" "a7" Malformed;
    parse_excep_test "An input of a 7 a 5 should raise Malformed" "a 7 a 5"
      Malformed;
  ]

let promotion_parse_tests =
  [
    promotion_parse_test "1 -> Queen" " 1    " Queen;
    promotion_parse_test "queen -> Queen" " queen    " Queen;
    promotion_parse_test "2 -> Rook" " 2    " Rook;
    promotion_parse_test "rook -> Rook" " rOok   " Rook;
    promotion_parse_test "3 -> Knight" " 3    " Knight;
    promotion_parse_test "knight -> Knight" " kNIght    " Knight;
    promotion_parse_test "4 -> Bishop" " 4    " Bishop;
    promotion_parse_test "bishop -> Bishop" " BiShop    " Bishop;
  ]

let suite =
  "test suite for Command"
  >::: List.flatten [ parse_tests; parse_excep_tests; promotion_parse_tests ]

let tests =
  List.flatten [ parse_tests; parse_excep_tests; promotion_parse_tests ]

let _ = run_test_tt_main suite
