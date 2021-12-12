open Board
open Piece

exception WrongColor

exception NoUndo

exception IllegalPromotion

type enemy_pawn_pos = int * int

type player_pawn_pos = int * int

type en_passant = (enemy_pawn_pos * player_pawn_pos) option

type result =
  | Playing of en_passant
  | Promotion of (int * int)
  | WhiteWin
  | BlackWin
  | Stalemate

type t = {
  board : Board.t;
  white_graveyard : Piece.t list;
  black_graveyard : Piece.t list;
  turn : bool;
  result : result;
  prev_state : t option;
}

let init_state =
  {
    board = init_board;
    turn = false;
    white_graveyard = [];
    black_graveyard = [];
    result = Playing None;
    prev_state = None;
  }

let turn s = s.turn

let board s = s.board

let result s = s.result

let graveyard s = function
  | true -> s.black_graveyard
  | false -> s.white_graveyard

let rec score_of acc = function
  | [] -> acc
  | h :: t -> score_of (acc + value h) t

let score s = function
  | true -> score_of 0 s.white_graveyard
  | false -> score_of 0 s.black_graveyard

let valid_pos (a, b) = a < 8 && a >= 0 && b < 8 && b >= 0

let rec move_out_of_check board piece = function
  | [] -> false
  | pos :: t ->
      let try_current_move =
        try move (position piece) pos None board |> fst with
        | InvalidPos -> board
      in
      if check try_current_move || try_current_move = board then
        move_out_of_check board piece t
      else true

let rec try_all_check_moves board = function
  | [] -> true
  | piece :: t ->
      if move_out_of_check board piece (next_moves board piece) then false
      else try_all_check_moves board t

let checkmate state =
  let board = board state in
  let turn = turn state in
  if board = init_board then false
  else
    let same_pieces = board |> if turn then black_pieces else white_pieces in
    try_all_check_moves board same_pieces && check board

let rec try_move board piece = function
  | [] -> false
  | pos :: t ->
      let result =
        try move (position piece) pos None board |> fst with
        | InvalidPos -> board
      in
      result = board

let stalemate_helper board turn =
  let pieces =
    (if turn then black_pieces board else white_pieces board) |> Array.of_list
  in
  let non_kings = ref [] in
  let king_pos = ref 0 in
  for i = 0 to Array.length pieces - 1 do
    if is_king pieces.(i) then king_pos := i
    else non_kings := pieces.(i) :: !non_kings
  done;
  let lst = List.map (next_moves board) !non_kings |> List.flatten in
  let king = pieces.(!king_pos) in
  lst = [] && try_all_check_moves board [ king ]

let stalemate state =
  let board = state.board in
  let turn = state.turn in
  stalemate_helper board turn && not (check board)

let en_passant_pawn_check board turn = function
  | [ pos1 ] ->
      let piece_at_pos1 = piece_at board pos1 in
      if is_pawn piece_at_pos1 && color piece_at_pos1 <> turn then Some pos1
      else None
  | [ pos1; pos2 ] ->
      let piece_at_pos1 = piece_at board pos1 in
      let piece_at_pos2 = piece_at board pos2 in
      if is_pawn piece_at_pos1 && color piece_at_pos1 <> turn then Some pos1
      else if is_pawn piece_at_pos2 && color piece_at_pos2 <> turn then
        Some pos2
      else None
  | _ -> raise WrongColor

let en_passant (startx, starty) (endx, endy) board turn =
  if starty = endy then
    let possible_pawn_positions =
      List.filter
        (fun pos -> valid_pos pos)
        [ (endx, endy + 1); (endx, endy - 1) ]
    in
    match turn with
    | true ->
        if endx - startx = 2 then
          match en_passant_pawn_check board turn possible_pawn_positions with
          | Some enemy -> Some enemy
          | None -> None
        else None
    | false ->
        if startx - endx = 2 then
          match en_passant_pawn_check board turn possible_pawn_positions with
          | Some enemy -> Some enemy
          | None -> None
        else None
  else None

let undo state =
  match state.prev_state with
  | None -> raise NoUndo
  | Some t -> t

let promotable piece (x, _) =
  if is_pawn piece then (color piece && x = 7) || ((not (color piece)) && x = 0)
  else false

let checkmate_state state new_state =
  if turn state then
    {
      new_state with
      white_graveyard = init_piece "king" false 0 0 :: new_state.white_graveyard;
      result = BlackWin;
    }
  else
    {
      new_state with
      black_graveyard = init_piece "king" true 0 0 :: new_state.black_graveyard;
      result = WhiteWin;
    }

let promotion_piece piece state =
  let previous_state = Some state in
  let pos =
    match state.result with
    | Promotion p -> p
    | _ -> raise IllegalPromotion
  in
  if pos |> piece_at state.board |> is_pawn |> not then raise IllegalPromotion
  else
    let new_board = Board.promotion pos state.board piece in
    let new_state =
      {
        state with
        board = new_board;
        prev_state = previous_state;
        result = Playing None;
        turn = not state.turn;
      }
    in
    if checkmate new_state then checkmate_state state new_state
    else if stalemate new_state then { new_state with result = Stalemate }
    else new_state

let promotion_state pos (previous_state, new_board, captured_piece, state) =
  let result_promo = Promotion pos in
  if turn state then
    {
      state with
      board = new_board;
      prev_state = previous_state;
      result = result_promo;
      white_graveyard =
        (if is_empty captured_piece then state.white_graveyard
        else captured_piece :: state.white_graveyard);
    }
  else
    {
      state with
      board = new_board;
      prev_state = previous_state;
      result = result_promo;
      black_graveyard =
        (if is_empty captured_piece then state.black_graveyard
        else captured_piece :: state.black_graveyard);
    }

let reg_state (previous_state, new_board, captured_piece, state) =
  if turn state then
    {
      state with
      board = new_board;
      turn = not state.turn;
      prev_state = previous_state;
      white_graveyard =
        (if is_empty captured_piece then state.white_graveyard
        else captured_piece :: state.white_graveyard);
    }
  else
    {
      state with
      board = new_board;
      turn = not state.turn;
      prev_state = previous_state;
      black_graveyard =
        (if is_empty captured_piece then state.black_graveyard
        else captured_piece :: state.black_graveyard);
    }

let state_en_passant_helper pos1 pos2 state new_state =
  let en_passant_enemy = en_passant pos1 pos2 state.board state.turn in
  match en_passant_enemy with
  | Some enemy_pos ->
      { new_state with result = Playing (Some (pos2, enemy_pos)) }
  | None -> { new_state with result = Playing None }

let change_state pos1 pos2 state =
  let currently_en_passant =
    match state.result with
    | Playing en_passant_position -> en_passant_position
    | _ -> None
  in
  let curr_board = board state in
  let curr_piece = piece_at curr_board pos1 in
  if turn state <> color curr_piece then raise WrongColor
  else
    let new_board, captured_piece =
      Board.move pos1 pos2 currently_en_passant curr_board
    in
    let key_args = (Some state, new_board, captured_piece, state) in
    let new_state =
      if promotable curr_piece pos2 then promotion_state pos2 key_args
      else reg_state key_args
    in
    if checkmate new_state then checkmate_state state new_state
    else if stalemate new_state then { new_state with result = Stalemate }
    else if new_state.result = Promotion pos2 then new_state
    else state_en_passant_helper pos1 pos2 state new_state
