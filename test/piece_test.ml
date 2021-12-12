open OUnit2
open Chess.Piece
open Values
open Helper

let position_test name expected_output piece =
  name >:: fun _ ->
  assert_equal expected_output (position piece) ~printer:position_printer

let name_test test_name expected_output piece =
  test_name >:: fun _ -> assert_equal expected_output (name piece) ~printer:id

let value_test name expected_output piece =
  name >:: fun _ ->
  assert_equal expected_output (value piece) ~printer:string_of_int

let is_king_test name expected_output piece =
  name >:: fun _ ->
  assert_equal expected_output (is_king piece) ~printer:string_of_bool

let is_pawn_test name expected_output piece =
  name >:: fun _ ->
  assert_equal expected_output (is_pawn piece) ~printer:string_of_bool

let moves_test name expected_output piece =
  name >:: fun _ ->
  assert_equal expected_output (moves piece) ~printer:string_of_int

let color_test name expected_output piece =
  name >:: fun _ ->
  assert_equal expected_output (color piece) ~printer:string_of_bool

let is_empty_test name expected_output piece =
  name >:: fun _ ->
  assert_equal expected_output (is_empty piece) ~printer:string_of_bool

let valid_moves_test name expected_output piece =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output (valid_moves piece)
    ~printer:(pp_list position_printer)

let position_tests =
  [
    position_test "position of black pawn is (1,0)" (1, 0) bl_pawn;
    position_test "position of empty square is (2,5)" (2, 5) empty_sq;
    position_test "position of moved pawn is (3,0)" (3, 0) moved_pawn;
    position_test "position of castled white king is (7,6)" (7, 6)
      castled_wh_king;
    position_test "position of castled black king is (0,6)" (0, 6)
      castled_bl_king;
    position_test "moving a pawn in-place 5 times has a position of (3,0)"
      (3, 0) moved_5_times;
  ]

let name_tests =
  [
    name_test "name of black pawn is ♟︎" "♟︎" bl_pawn;
    name_test "name of white pawn is ♙" "♙" wh_pawn;
    name_test "name of empty square is [ ]" " " empty_sq;
    name_test "name of white rook is ♖" "♖" wh_rook;
    name_test "name of black rook is ♜" "♜" bl_rook;
    name_test "name of black queen is ♛" "♛" bl_queen;
    name_test "name of white queen is ♕" "♕" wh_queen;
    name_test "name of left white knight is ♘" "♘" initial_wh_knight_L;
    name_test "name of right white knight is ♘" "♘" initial_wh_knight_R;
    name_test "name of black knight is ♞" "♞" bl_knight;
    name_test "name of black bishop is ♝" "♝" bl_bishop;
    name_test "name of white bishop is ♗" "♗" wh_bishop;
    name_test "name of white king is ♔" "♔" wh_king;
    name_test "name of castled white king is ♔" "♔" castled_wh_king;
    name_test "name of black king is ♚" "♚" bl_king;
    name_test "name of castled black king is ♚" "♚" castled_bl_king;
  ]

let value_tests =
  [
    value_test "value of black pawn is 1" 1 bl_pawn;
    value_test "value of white pawn is 1" 1 wh_pawn;
    value_test "value of black rook is 5" 5 bl_rook;
    value_test "value of white rook is 5" 5 wh_rook;
    value_test "value of black bishop is 3" 3 bl_bishop;
    value_test "value of white bishop is 3" 3 wh_bishop;
    value_test "value of black knight is 3" 3 bl_knight;
    value_test "value of white knight is 3" 3 initial_wh_knight_L;
    value_test "value of black king is 1000" 1000 bl_king;
    value_test "value of white king is 1000" 1000 wh_king;
    value_test "value of black queen is 9" 9 bl_queen;
    value_test "value of white queen is 9" 9 wh_queen;
    value_test "value of empty is 0" 0 empty_sq;
  ]

let is_pawn_tests =
  [
    is_pawn_test "A black pawn is a pawn" true bl_pawn;
    is_pawn_test "A white pawn is a pawn" true wh_pawn;
    is_pawn_test "A black pawn is a pawn" true kingside_bl_pawn;
    is_pawn_test "A white rook is not a pawn" false wh_rook;
    is_pawn_test "A black king is not a pawn" false bl_king;
    is_pawn_test "A white king is not a pawn" false wh_king;
    is_pawn_test "A white queen is not a pawn" false wh_queen;
    is_pawn_test "A black knight is not a pawn" false bl_knight;
    is_pawn_test "A black bishop is not a pawn" false bl_bishop;
    is_pawn_test "A black queen is not a pawn" false bl_queen;
    is_pawn_test "A white knight is not a pawn" false initial_wh_knight_L;
    is_pawn_test "A white knight is not a pawn" false initial_wh_knight_R;
    is_pawn_test "A black king is not a pawn" false castled_bl_king;
    is_pawn_test "A white king is not a pawn" false castled_wh_king;
    is_pawn_test "A black king is not a pawn" false long_castled_bl_king;
    is_pawn_test "A white king is not a pawn" false long_castled_wh_king;
    is_pawn_test "A black king is not a pawn" false checkmated_bl_king;
    is_pawn_test "A white king is not a pawn" false checkmated_bl_king;
  ]

let is_king_tests =
  [
    is_king_test "A white pawn is not a king" false wh_pawn;
    is_king_test "A white rook is not a king" false wh_rook;
    is_king_test "A black rook is not a king" false bl_rook;
    is_king_test "A black knight is not a king" false bl_knight;
    is_king_test "A black bishop is not a king" false bl_bishop;
    is_king_test "A black queen is not a king" false bl_queen;
    is_king_test "A white knight is not a king" false initial_wh_knight_R;
    is_king_test "A black king is a king" true castled_bl_king;
    is_king_test "A white king is a king" true castled_wh_king;
    is_king_test "A black king is a king" true long_castled_bl_king;
    is_king_test "A white king is a king" true long_castled_wh_king;
    is_king_test "A black king is true" true bl_king;
    is_king_test "A white king is true" true wh_king;
    is_king_test "A black queen is false" false bl_queen;
    is_king_test "A white queen is not a king" false wh_queen;
    is_king_test "A black pawn should return not false" false bl_pawn;
    is_king_test "A white knight is false" false initial_wh_knight_L;
  ]

let moves_tests =
  [
    moves_test "moves of an initial black pawn is 0" 0 bl_pawn;
    moves_test "moves of an initial black pawn is 0" 0 bl_knight;
    moves_test "moves of an initial white knight is 0" 0 initial_wh_knight_L;
    moves_test "moves of an initial white knight is 0" 0 initial_wh_knight_R;
    moves_test "moves of an initial black bishop is 0" 0 bl_bishop;
    moves_test "moves of an initial black queen is 0" 0 bl_queen;
    moves_test "moves of an initial black queen is 0" 0 initial_bl_queen;
    moves_test "moves of an initial white king is 0" 0 wh_king;
    moves_test "moves of an initial white bishop is 0" 0 wh_bishop;
    moves_test "moves of an initial black rook is 0" 0 bl_rook;
    moves_test "moves of a castled white king is 1" 1 castled_wh_king;
    moves_test "moves of an castled black king is 1" 1 castled_bl_king;
    moves_test "moves of an long castled black king is 1" 1 long_castled_bl_king;
    moves_test "moves of an long castled white king is 1" 1 long_castled_wh_king;
    moves_test "moves of an checkmated white king is 0" 0 checkmated_wh_king;
    moves_test "moves of a black king is 1" 1 bl_king;
    moves_test "moved pawn's move counter is 1" 1 moved_pawn;
    moves_test "moving a pawn 5 times has counter of 5" 5 moved_5_times;
    moves_test "moves of a promotion queen is 5" 5 promotion_queen;
  ]

let color_tests =
  [
    color_test "color of black pawn is true (black)" true bl_pawn;
    color_test "color of white pawn is false (white)" false wh_pawn;
    color_test "color of white rook is false (white)" false wh_rook;
    color_test "color of black king is true (black)" true bl_king;
    color_test "color of white knight is false (white)" false
      initial_wh_knight_L;
    color_test "color of white knight is false (white)" false
      initial_wh_knight_R;
    color_test "color of white king is false (white)" false castled_wh_king;
    color_test "color of black king is true (black)" true castled_bl_king;
    color_test "color of black king is true (black)" true long_castled_bl_king;
    color_test "color of white king is false (white)" false long_castled_wh_king;
    color_test "color of white knight is false (white)" false
      initial_wh_knight_L;
    color_test "color of white king is false (white)" false checkmated_wh_king;
    color_test "color of black king is true (black)" true checkmated_bl_king;
    color_test "color of black pawn is true (black)" true kingside_bl_pawn;
    color_test "color of black queen is true (black)" true initial_bl_queen;
    color_test "color of black bishop is true (black)" true bl_bishop;
    color_test "color of black queen is true (black)" true bl_queen;
  ]

let is_empty_tests =
  [
    is_empty_test "black pawn is not empty" false bl_pawn;
    is_empty_test "black queen is not empty" false bl_queen;
    is_empty_test "black king is not empty" false bl_king;
    is_empty_test "black rook is not empty" false bl_rook;
    is_empty_test "white queen is not empty" false wh_queen;
    is_empty_test "white bishop is not empty" false wh_bishop;
    is_empty_test "white king is not empty" false wh_king;
    is_empty_test "white rook is not empty" false wh_rook;
    is_empty_test "black knight is not empty" false bl_knight;
    is_empty_test "white pawn is not empty" false wh_pawn;
    is_empty_test "moved pawn is not empty" false moved_pawn;
    is_empty_test "white knight is not empty" false initial_wh_knight_L;
    is_empty_test "black bishop is not empty" false bl_bishop;
    is_empty_test "black queen is not empty" false initial_bl_queen;
    is_empty_test "promotion queen is not empty" false promotion_queen;
    is_empty_test "empty square is empty" true empty_sq;
  ]

let valid_moves_tests =
  [
    valid_moves_test "valid move of black pawn is [(2,0); (3,0);(2,1)]"
      [ (2, 0); (3, 0); (2, 1) ] bl_pawn;
    valid_moves_test "valid move of white pawn is [(5, 0); (4,0); (5,1)] "
      [ (5, 0); (4, 0); (5, 1) ] wh_pawn;
    valid_moves_test
      "valid move of king is [(1,3);(1,4);(1,5);(2,3);(2,5);(3,3);(3,4);(3,5)]"
      [ (1, 3); (1, 4); (1, 5); (2, 3); (2, 5); (3, 3); (3, 4); (3, 5) ]
      bl_king;
    valid_moves_test
      "valid moves of rook is [ (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, \
       0); (7,0); (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7)]"
      [
        (1, 0);
        (2, 0);
        (3, 0);
        (4, 0);
        (5, 0);
        (6, 0);
        (7, 0);
        (0, 1);
        (0, 2);
        (0, 3);
        (0, 4);
        (0, 5);
        (0, 6);
        (0, 7);
      ]
      bl_rook;
    valid_moves_test "valid moves of knight is [(2,0);(2,2); (1,3)]"
      [ (2, 0); (2, 2); (1, 3) ] bl_knight;
    valid_moves_test
      "valid moves of bishop is [(1,1);(2,0);(1,3);(2,4);(3,5);(4,6);(5,7)]"
      [ (1, 1); (2, 0); (1, 3); (2, 4); (3, 5); (4, 6); (5, 7) ]
      bl_bishop;
    valid_moves_test
      "valid moves of queen is \
       [(3,0);(3,1);(3,2);(3,4);(3,5);(3,6);(3,7);(0,3);(1,3);(2,3);(4,3);(4,2);(5,3);(6,3);(7,3);(0,0);(1,1);(2,2);(4,4);(5,5);(6,6);(7,7);(0,6);(1,5);(2,4);(5,1);(6,0)]"
      [
        (3, 0);
        (3, 1);
        (3, 2);
        (3, 4);
        (3, 5);
        (3, 6);
        (3, 7);
        (0, 3);
        (1, 3);
        (2, 3);
        (4, 3);
        (5, 3);
        (6, 3);
        (7, 3);
        (0, 0);
        (1, 1);
        (2, 2);
        (4, 4);
        (5, 5);
        (6, 6);
        (7, 7);
        (0, 6);
        (1, 5);
        (2, 4);
        (4, 2);
        (5, 1);
        (6, 0);
      ]
      bl_queen;
    valid_moves_test
      "valid moves of initial queen is [(0,0);(0,1);(0,2); (0,4); \
       (0,5);(0,6);(0,7);(1,3);(2,3);(3,3);(4,3);(5,3);(6,3);(7,3);(1,2);(2,1);(3,0);(1,4);(2,5);(3,6);(4,7)]"
      [
        (0, 0);
        (0, 1);
        (0, 2);
        (0, 4);
        (0, 5);
        (0, 6);
        (0, 7);
        (1, 3);
        (2, 3);
        (3, 3);
        (4, 3);
        (5, 3);
        (6, 3);
        (7, 3);
        (1, 2);
        (2, 1);
        (3, 0);
        (1, 4);
        (2, 5);
        (3, 6);
        (4, 7);
      ]
      initial_bl_queen;
  ]

let suite =
  "test suite for Piece"
  >::: List.flatten
         [
           is_king_tests;
           is_pawn_tests;
           position_tests;
           name_tests;
           value_tests;
           moves_tests;
           color_tests;
           is_empty_tests;
           valid_moves_tests;
         ]

let tests =
  List.flatten
    [
      is_king_tests;
      is_pawn_tests;
      position_tests;
      name_tests;
      value_tests;
      moves_tests;
      color_tests;
      is_empty_tests;
      valid_moves_tests;
    ]

let _ = run_test_tt_main suite
