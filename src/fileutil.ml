open State
open Command
open Piece

exception NotFound

exception InvalidText of int

exception InvalidPos of int

exception WrongColor of int

exception InvalidPromotion of int

let data_dir_prefix = "data" ^ Filename.dir_sep

let promote_helper state str line (x, y) =
  match promotion_parse str with
  | Knight -> promotion_piece (init_piece "knight" (turn state) x y) state
  | Rook -> promotion_piece (init_piece "rook" (turn state) x y) state
  | Queen -> promotion_piece (init_piece "queen" (turn state) x y) state
  | Bishop -> promotion_piece (init_piece "bishop" (turn state) x y) state

let parse_line state str line =
  match result state with
  | Promotion pos -> promote_helper state str line pos
  | _ -> begin
      match parse str with
      | Move (pos1, pos2) -> change_state pos1 pos2 state
      | _ -> raise (InvalidText line)
    end

let rec go_thru_state state line = function
  | [] -> state
  | h :: t ->
      let new_state =
        try parse_line state h line with
        | Malformed -> raise (InvalidText line)
        | Board.InvalidPos -> raise (InvalidPos line)
        | State.WrongColor -> raise (WrongColor line)
        | IllegalPromotion -> raise (InvalidPromotion line)
      in
      go_thru_state new_state (line + 1) t

let rec read_each_line channel acc =
  try
    read_each_line channel
      ((input_line channel |> String.lowercase_ascii) :: acc)
  with
  | End_of_file ->
      close_in channel;
      acc

let config file =
  let channel =
    try open_in (data_dir_prefix ^ file ^ ".txt") with
    | _ -> raise NotFound
  in
  List.rev (read_each_line channel []) |> go_thru_state init_state 1