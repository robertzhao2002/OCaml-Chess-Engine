open Piece

type t = Piece.t array array

exception InvalidPos

(* Helper functions *)
let invalid_pos (x, y) = x < 0 || x > 7 || y < 0 || y > 7

let rec replace_piece piece x y board =
  let new_board = Array.map Array.copy board in
  new_board.(x).(y) <- piece;
  new_board

let piece_at board (x, y) =
  if invalid_pos (x, y) then raise InvalidPos else board.(x).(y)

let castle_short board piece x y =
  let rook = piece_at board (x, 7) in
  let old_x, old_y = position piece in
  ( replace_piece (move_piece (x, y) piece) x y board
    |> replace_piece (init_piece "empty" false old_x old_y) old_x old_y
    |> replace_piece (move_piece (x, 5) rook) x 5
    |> replace_piece (init_piece "empty" false x 7) x 7,
    init_piece "empty" false x y )

let castle_long board piece x y =
  let rook = piece_at board (x, 0) in
  let old_x, old_y = position piece in
  ( replace_piece (move_piece (x, y) piece) x y board
    |> replace_piece (init_piece "empty" false old_x old_y) old_x old_y
    |> replace_piece (move_piece (x, 3) rook) x 3
    |> replace_piece (init_piece "empty" false x 0) x 0,
    init_piece "empty" false x y )

let row_to_string row num =
  Array.fold_left
    (fun acc piece -> acc ^ name piece ^ " |")
    (string_of_int num ^ " |")
    row

let empty_squares x = Array.init 8 (fun y -> init_piece "empty" false x y)

let pawns color x = Array.init 8 (fun y -> init_piece "pawn" color x y)

let backrank color x =
  [|
    init_piece "rook" color x 0;
    init_piece "knight" color x 1;
    init_piece "bishop" color x 2;
    init_piece "queen" color x 3;
    init_piece "king" color x 4;
    init_piece "bishop" color x 5;
    init_piece "knight" color x 6;
    init_piece "rook" color x 7;
  |]

let init_board =
  [|
    backrank true 0;
    pawns true 1;
    empty_squares 2;
    empty_squares 3;
    empty_squares 4;
    empty_squares 5;
    pawns false 6;
    backrank false 7;
  |]

let flatten board =
  let flattened = ref [||] in
  for i = 0 to Array.length board - 1 do
    flattened := Array.append !flattened board.(i)
  done;
  !flattened

let pieces_with_condition condition board =
  let pieces = flatten board |> Array.to_list in
  List.filter condition pieces

let black_pieces board =
  pieces_with_condition
    (fun piece -> (not (is_empty piece)) && color piece)
    board

let white_pieces board =
  pieces_with_condition (fun piece -> not (is_empty piece || color piece)) board

let empty_position board (x, y) = is_empty (piece_at board (x, y))

let enemy_position board color (x, y) =
  let enemy_color = (x, y) |> piece_at board |> Piece.color in
  (not (empty_position board (x, y))) && color <> enemy_color

let rec valid_check board piece_color (x, y) transformation lst =
  let x2, y2 = transformation (x, y) in
  if x2 < 0 || x2 > 7 || y2 < 0 || y2 > 7 then lst
  else
    let pos_added = (x2, y2) :: lst in
    if empty_position board (x2, y2) then
      valid_check board piece_color (x2, y2) transformation pos_added
    else if enemy_position board piece_color (x2, y2) then pos_added
    else lst

let pawn_moves board piece (x, y) =
  let init_x, init_y = position piece in
  let diagonal_check (startx, starty) (endx, endy) =
    (endx = 1 + startx && endy = 1 + starty)
    || (endx = 1 + startx && endy = starty - 1)
    || (endx = startx - 1 && endy = starty + 1)
    || (endx = startx - 1 && endy = starty - 1)
  in
  if diagonal_check (init_x, init_y) (x, y) then
    enemy_position board (Piece.color piece) (x, y)
  else (x, y) |> piece_at board |> is_empty

let rec check_is_empty board = function
  | [] -> true
  | h :: t -> empty_position board h && check_is_empty board t

let castling_filter board color long =
  let positions_to_check, rook_pos =
    match long with
    | true -> begin
        match color with
        | true -> ([ (0, 1); (0, 2); (0, 3) ], (0, 0))
        | false -> ([ (7, 1); (7, 2); (7, 3) ], (7, 0))
      end
    | false -> begin
        match color with
        | true -> ([ (0, 5); (0, 6) ], (0, 7))
        | false -> ([ (7, 5); (7, 6) ], (7, 7))
      end
  in
  let rook = piece_at board rook_pos in
  check_is_empty board positions_to_check
  && (name rook = "♖" || name rook = "♜")
  && moves rook = 0
  && Piece.color rook = color

let king_moves board piece (x, y) =
  let regular_filter =
    empty_position board (x, y)
    || enemy_position board (Piece.color piece) (x, y)
  in
  if moves piece = 0 then
    match (x, y) with
    | 0, 2 -> castling_filter board true true
    | 0, 6 -> castling_filter board true false
    | 7, 2 -> castling_filter board false true
    | 7, 6 -> castling_filter board false false
    | _ -> regular_filter
  else regular_filter

let move_filter board piece =
  if is_pawn piece then List.filter (fun pos -> pawn_moves board piece pos)
  else if is_king piece then List.filter (fun pos -> king_moves board piece pos)
  else if Piece.name piece = "♞" || Piece.name piece = "♘" then
    List.filter (fun pos ->
        empty_position board pos || enemy_position board (Piece.color piece) pos)
  else
    let position = Piece.position piece in
    let color = Piece.color piece in
    let total_lst =
      valid_check board color position (fun (x, y) -> (x - 1, y)) []
      |> valid_check board color position (fun (x, y) -> (x + 1, y))
      |> valid_check board color position (fun (x, y) -> (x, y - 1))
      |> valid_check board color position (fun (x, y) -> (x, y + 1))
      |> valid_check board color position (fun (x, y) -> (x - 1, y - 1))
      |> valid_check board color position (fun (x, y) -> (x - 1, y + 1))
      |> valid_check board color position (fun (x, y) -> (x + 1, y - 1))
      |> valid_check board color position (fun (x, y) -> (x + 1, y + 1))
    in
    List.filter (fun x -> List.mem x total_lst)

let next_moves board piece =
  let possible_moves =
    try valid_moves piece with
    | EmptySquare -> []
  in
  move_filter board piece possible_moves

let rec enemy_king_check board curr_color = function
  | [] -> false
  | pos :: t ->
      let piece_at_pos = piece_at board pos in
      if is_king piece_at_pos then
        let piece_at_pos_color = color piece_at_pos in
        if piece_at_pos_color <> curr_color then true
        else enemy_king_check board curr_color t
      else enemy_king_check board curr_color t

let rec enemy_under_check board = function
  | [] -> false
  | h :: t ->
      let piece_possible_moves = next_moves board h in
      let piece_color = color h in
      if enemy_king_check board piece_color piece_possible_moves then true
      else enemy_under_check board t

let check board = board |> flatten |> Array.to_list |> enemy_under_check board

let move_into_check board (x, y) enemy_pieces =
  enemy_pieces
  |> List.map (fun piece ->
         if is_pawn piece then valid_moves piece else next_moves board piece)
  |> List.flatten |> List.sort_uniq compare
  |> List.mem (x, y)

let castling_move (x1, y1) (x2, y2) board piece enemy_pieces =
  let middle_pos = if y2 < y1 then (x1, y1 - 1) else (x1, y1 + 1) in
  if
    enemy_under_check board enemy_pieces
    || move_into_check board middle_pos enemy_pieces
    || move_into_check board (x2, y2) enemy_pieces
  then raise InvalidPos
  else if y2 - y1 = -2 then castle_long board piece x2 y2
  else castle_short board piece x2 y2

let regular_move (x1, y1) (x2, y2) piece board enemy_pieces =
  if next_moves board piece |> List.mem (x2, y2) |> not then raise InvalidPos
  else
    let castling board piece (x1, y1) (x2, y2) =
      is_king piece && y1 - y2 |> abs = 2 && moves piece = 0
    in
    if castling board piece (x1, y1) (x2, y2) then
      castling_move (x1, y1) (x2, y2) board piece enemy_pieces
    else
      let captured_piece = piece_at board (x2, y2) in
      ( replace_piece (init_piece "empty" false x1 y1) x1 y1 board
        |> replace_piece (move_piece (x2, y2) piece) x2 y2,
        captured_piece )

let en_passant_helper
    (x1, y1)
    (x2, y2)
    prev_pawn_pos
    enemy_pawn_pos
    piece
    board
    enemies =
  let prev_pawn_row = fst prev_pawn_pos in
  let prev_pawn_col = snd prev_pawn_pos in
  if (x1, y1) = enemy_pawn_pos then
    let pawn_initial = piece_at board (x1, y1) in
    let pawn_moved = move_piece (x2, y2) pawn_initial in
    ( replace_piece
        (init_piece "empty" false prev_pawn_row prev_pawn_col)
        prev_pawn_row prev_pawn_col
        (board
        |> replace_piece (init_piece "empty" false x1 y1) x1 y1
        |> replace_piece pawn_moved x2 y2),
      piece_at board prev_pawn_pos )
  else regular_move (x1, y1) (x2, y2) piece board enemies

let enemy_pieces color board =
  if color then white_pieces board else black_pieces board

let promotion (x, y) board piece = replace_piece piece x y board

let move (x1, y1) (x2, y2) en_passant board =
  let curr_piece = (x1, y1) |> piece_at board in
  let piece_color = Piece.color curr_piece in
  let enemy_pieces_list = enemy_pieces piece_color board in
  let new_board, captured =
    match en_passant with
    | Some (prev_pawn_pos, enemy_pawn_pos) ->
        en_passant_helper (x1, y1) (x2, y2) prev_pawn_pos enemy_pawn_pos
          curr_piece board enemy_pieces_list
    | None -> regular_move (x1, y1) (x2, y2) curr_piece board enemy_pieces_list
  in
  let new_enemy_pieces = enemy_pieces piece_color new_board in
  if enemy_under_check new_board new_enemy_pieces then raise InvalidPos
  else (new_board, captured)

let sep = "\n  -------------------------\n"

let rec to_string_helper board num =
  let board_string = ref "" in
  for i = 0 to Array.length board - 1 do
    board_string := !board_string ^ row_to_string board.(i) (8 - i) ^ sep
  done;
  !board_string

let rec to_string (board : t) =
  "   A  B  C  D  E  F  G  H " ^ sep ^ to_string_helper board 8
