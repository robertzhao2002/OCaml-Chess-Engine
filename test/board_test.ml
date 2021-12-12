open OUnit2
open Chess.Board
open Chess.Piece
open Helper
open Values

let piece_at_test name expected_output board pos =
  name >:: fun _ -> assert_equal expected_output (piece_at board pos)

let white_pieces_test name expected_output board =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output (white_pieces board)

let black_pieces_test name expected_output board =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output (black_pieces board)

let next_moves_test name expected_output board piece =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output (next_moves board piece)
    ~printer:(pp_list position_printer)

let invalidpos_test name board curr_pos new_pos =
  name >:: fun _ ->
  assert_raises InvalidPos (fun () -> move curr_pos new_pos None board)

let check_test name board expected_output =
  name >:: fun _ -> check board |> assert_equal expected_output

let to_string_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (to_string input) ~printer:id

let piece_at_tests =
  [
    piece_at_test "the piece at (7, 6) after white castles short is a king"
      castled_wh_king wh_sh_castle_board (7, 6);
    piece_at_test "the piece at (0, 6) after black castles short is a king"
      castled_bl_king bl_sh_castle_board (0, 6);
    piece_at_test "the piece at (7, 2) after white castles long is a king"
      long_castled_wh_king wh_lg_castle_board (7, 2);
    piece_at_test "the piece at (0, 2) after black castles long is a king"
      long_castled_bl_king bl_lg_castle_board (0, 2);
    piece_at_test
      "the piece at (0, 5) after white promotes to a queen is a white queen"
      (init_piece "queen" false 0 5)
      promote_into_check (0, 5);
    piece_at_test
      "the piece at (0, 5) after white promotes to a knight is a white knight"
      (init_piece "knight" false 0 5)
      promote_not_into_check (0, 5);
    piece_at_test "the piece at (0, 6) after black castles short is a king"
      castled_bl_king bl_sh_castle_board (0, 6);
    piece_at_test "the piece at (7, 2) after white castles long is a king"
      long_castled_wh_king wh_lg_castle_board (7, 2);
    piece_at_test "the piece at (0, 2) after black castles long is a king"
      long_castled_bl_king bl_lg_castle_board (0, 2);
  ]

let next_moves_tests =
  [
    next_moves_test "black pawn's next move is [(2, 0); (3,0)] "
      [ (2, 0); (3, 0) ] init_board bl_pawn;
    next_moves_test "initial black queen's next move is []" [] init_board
      initial_bl_queen;
    next_moves_test "initial black bishop's next move is []" [] init_board
      bl_bishop;
    next_moves_test
      "black rook's next moves is [(2, 0); (3, 0); (4, 0); (5, 0)]" []
      initial_board bl_rook;
    next_moves_test "left white knight's next moves is [(5, 0); (5, 2)]"
      [ (5, 0); (5, 2) ] initial_board initial_wh_knight_L;
    next_moves_test "right white knight's next moves is [(5, 7); (5, 5)]"
      [ (5, 7); (5, 5) ] initial_board initial_wh_knight_R;
    next_moves_test "white pawn's next moves is [(5, 0); (4,0)] "
      [ (5, 0); (4, 0) ] initial_board wh_pawn;
    next_moves_test
      "white rook's next moves after moving a pawn is [(6,0); (5,0)]"
      [ (6, 0); (5, 0) ]
      (move (6, 0) (4, 0) None initial_board |> fst)
      wh_rook;
  ]

let get_color_pieces_tests =
  [
    white_pieces_test "white pieces of initial board" initial_white_pieces
      initial_board;
    black_pieces_test "all black pieces of initial board" initial_black_pieces
      initial_board;
  ]

let invalidpos_tests =
  [
    invalidpos_test "moving to (-1,-1) should raise InvalidPos" initial_board
      (0, 0) (-1, -1);
    invalidpos_test "Cannot move into a check" move_into_check (1, 3) (3, 3);
    invalidpos_test "Cannot castle after moving the king" cannot_move_castle
      (7, 4) (7, 6);
    invalidpos_test "Cannot castle while under check" cannot_check_castle (7, 4)
      (7, 6);
    invalidpos_test "Cannot move into a check while castling"
      cannot_into_check_castle (7, 4) (7, 6);
    invalidpos_test "Cannot move through a check square while castling"
      check_square_castle (7, 4) (7, 6);
    invalidpos_test "Cannot ignore a checking piece" promote_into_check (0, 6)
      (0, 7);
  ]

let check_tests =
  [
    check_test "Initial board should not be in a check state" initial_board
      false;
    check_test "Scholar's checkmate should be in a check state" scholar_check
      true;
    check_test "Double check should be in a check state" double_check true;
    check_test "en passant into check should be in a check state"
      en_passant_check true;
    check_test "trying to castle while in check should be in a check state"
      cannot_check_castle true;
    check_test "promoting into a queen is a check state" promote_into_check true;
    check_test "promoting into a knight is not a check state"
      promote_not_into_check false;
  ]

let to_string_tests =
  [
    to_string_test "initial board's string configuration" initial_board_string
      initial_board;
    to_string_test "first board's configuation after moving white pawn at (6,4)"
      fst_board_string fst_board;
    to_string_test
      "second board's configuration after moving black pawn at (1,0)"
      snd_board_string snd_board;
  ]

let suite =
  "test suite for Board"
  >::: List.flatten
         [
           piece_at_tests;
           get_color_pieces_tests;
           next_moves_tests;
           invalidpos_tests;
           check_tests;
           to_string_tests;
         ]

let tests =
  List.flatten
    [
      piece_at_tests;
      get_color_pieces_tests;
      next_moves_tests;
      invalidpos_tests;
      check_tests;
      to_string_tests;
    ]

let _ = run_test_tt_main suite
