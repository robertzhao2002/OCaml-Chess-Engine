type color = bool

exception EmptySquare

exception InvalidPiece of string

type piece_info = {
  name : string;
  color : color;
  moves : int;
  x : int;
  y : int;
  value : int;
}

type t =
  | Pawn of piece_info
  | Rook of piece_info
  | Knight of piece_info
  | Bishop of piece_info
  | Queen of piece_info
  | King of piece_info
  | Empty of piece_info

(** [pattern_helper f piece] is the result of applying function [f] on [piece]
    by pattern-matching [piece] against the possible pieces. *)
let pattern_helper f = function
  | Pawn t -> f t
  | Rook t -> f t
  | Knight t -> f t
  | Bishop t -> f t
  | Queen t -> f t
  | King t -> f t
  | Empty t -> f t

let init_piece name color x y =
  match String.lowercase_ascii name with
  | "pawn" ->
      let piece_name = if color then "♟︎" else "♙" in
      Pawn { name = piece_name; moves = 0; color; x; y; value = 1 }
  | "knight" ->
      let piece_name = if color then "♞" else "♘" in
      Knight { name = piece_name; moves = 0; color; x; y; value = 3 }
  | "rook" ->
      let piece_name = if color then "♜" else "♖" in
      Rook { name = piece_name; moves = 0; color; x; y; value = 5 }
  | "bishop" ->
      let piece_name = if color then "♝" else "♗" in
      Bishop { name = piece_name; moves = 0; color; x; y; value = 3 }
  | "king" ->
      let piece_name = if color then "♚" else "♔" in
      King { name = piece_name; moves = 0; color; x; y; value = 1000 }
  | "queen" ->
      let piece_name = if color then "♛" else "♕" in
      Queen { name = piece_name; moves = 0; color; x; y; value = 9 }
  | "empty" -> Empty { name = " "; moves = 0; color; x; y; value = 0 }
  | _ -> raise (InvalidPiece name)

let is_empty piece =
  match piece with
  | Empty _ -> true
  | _ -> false

let position piece =
  pattern_helper (fun piece_info -> (piece_info.x, piece_info.y)) piece

let name piece = pattern_helper (fun piece_info -> piece_info.name) piece

let value p = pattern_helper (fun piece -> piece.value) p

let is_king = function
  | King _ -> true
  | _ -> false

let is_pawn = function
  | Pawn _ -> true
  | _ -> false

let color piece = pattern_helper (fun piece_info -> piece_info.color) piece

let moves piece = pattern_helper (fun piece_info -> piece_info.moves) piece

let valid_pos (x, y) = x <= 7 && x >= 0 && y >= 0 && y <= 7

let valid_pawn_moves (pawn : piece_info) : (int * int) list =
  let black_moves =
    List.filter
      (fun pos -> valid_pos pos)
      [
        (pawn.x + 1, pawn.y); (pawn.x + 1, pawn.y + 1); (pawn.x + 1, pawn.y - 1);
      ]
  in
  let white_moves =
    List.filter
      (fun pos -> valid_pos pos)
      [
        (pawn.x - 1, pawn.y); (pawn.x - 1, pawn.y + 1); (pawn.x - 1, pawn.y - 1);
      ]
  in
  match pawn.color with
  | true ->
      if pawn.moves = 0 then (pawn.x + 2, pawn.y) :: black_moves
      else black_moves
  | false ->
      if pawn.moves = 0 then (pawn.x - 2, pawn.y) :: white_moves
      else white_moves

let rec rook_updown row y current_list =
  if row <= 7 then rook_updown (row + 1) y ((row, y) :: current_list)
  else current_list

let rec rook_leftright col x current_list =
  if col <= 7 then rook_leftright (col + 1) x ((x, col) :: current_list)
  else current_list

let valid_rook_moves rook =
  let updown_list = rook_updown 0 in
  let left_right_list = rook_leftright 0 in
  let regular_moves =
    [] |> updown_list rook.y |> left_right_list rook.x
    |> List.filter (fun x -> x <> (rook.x, rook.y))
  in
  (if rook.moves = 0 then
   if rook.y = 0 then (rook.x, rook.y + 3) :: regular_moves
   else (rook.x, rook.y - 2) :: regular_moves
  else regular_moves)
  |> List.sort_uniq compare

let valid_knight_moves (knight : piece_info) : (int * int) list =
  [
    (knight.x + 1, knight.y + 2);
    (knight.x + 1, knight.y - 2);
    (knight.x - 1, knight.y + 2);
    (knight.x - 1, knight.y - 2);
    (knight.x + 2, knight.y + 1);
    (knight.x - 2, knight.y + 1);
    (knight.x + 2, knight.y - 1);
    (knight.x - 2, knight.y - 1);
  ]

let rec bishop_moves direction (x, y) current_list : (int * int) list =
  let next_direction = direction + 4 in
  match direction mod 4 with
  | 0 ->
      if direction / 4 <= 7 then
        (x + 1, y + 1)
        :: bishop_moves next_direction (x + 1, y + 1) current_list
      else current_list
  | 1 ->
      if direction / 4 <= 7 then
        (x + 1, y - 1)
        :: bishop_moves next_direction (x + 1, y - 1) current_list
      else current_list
  | 2 ->
      if direction / 4 <= 7 then
        (x - 1, y + 1)
        :: bishop_moves next_direction (x - 1, y + 1) current_list
      else current_list
  | _ ->
      if direction / 4 <= 7 then
        (x - 1, y - 1)
        :: bishop_moves next_direction (x - 1, y - 1) current_list
      else current_list

let valid_bishop_moves (bishop : piece_info) : (int * int) list =
  let top_left_list = bishop_moves 3 in
  let top_right_list = bishop_moves 2 in
  let bottom_left_list = bishop_moves 1 in
  let bottom_right_list = bishop_moves 0 in
  []
  |> top_left_list (bishop.x, bishop.y)
  |> top_right_list (bishop.x, bishop.y)
  |> bottom_left_list (bishop.x, bishop.y)
  |> bottom_right_list (bishop.x, bishop.y)
  |> List.sort_uniq compare

let valid_queen_moves (queen : piece_info) : (int * int) list =
  queen |> valid_rook_moves
  |> List.rev_append (valid_bishop_moves queen)
  |> List.sort_uniq compare

let valid_king_moves (king : piece_info) : (int * int) list =
  let regular_moves =
    [
      (king.x + 1, king.y);
      (king.x - 1, king.y);
      (king.x, king.y - 1);
      (king.x, king.y + 1);
      (king.x + 1, king.y + 1);
      (king.x + 1, king.y - 1);
      (king.x - 1, king.y + 1);
      (king.x - 1, king.y - 1);
    ]
  in
  if king.moves = 0 then
    (king.x, king.y + 2) :: (king.x, king.y - 2) :: regular_moves
  else regular_moves

let valid_moves = function
  | Pawn t -> List.filter valid_pos (valid_pawn_moves t)
  | Rook t -> List.filter valid_pos (valid_rook_moves t)
  | Knight t -> List.filter valid_pos (valid_knight_moves t)
  | Bishop t -> List.filter valid_pos (valid_bishop_moves t)
  | Queen t -> List.filter valid_pos (valid_queen_moves t)
  | King t -> List.filter valid_pos (valid_king_moves t)
  | _ -> raise EmptySquare

let move_piece (x, y) = function
  | Pawn t -> Pawn { t with moves = t.moves + 1; x; y }
  | Knight t -> Knight { t with moves = t.moves + 1; x; y }
  | Rook t -> Rook { t with moves = t.moves + 1; x; y }
  | Bishop t -> Bishop { t with moves = t.moves + 1; x; y }
  | King t -> King { t with moves = t.moves + 1; x; y }
  | Queen t -> Queen { t with moves = t.moves + 1; x; y }
  | Empty t -> Empty { t with moves = t.moves + 1; x; y }
