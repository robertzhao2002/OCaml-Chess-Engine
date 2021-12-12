exception Malformed

type position = int * int

type command =
  | Move of position * position
  | Reset
  | Quit
  | Undo
  | Help
  | Rules
  | Draw
  | Score

type promotion_pieces =
  | Knight
  | Rook
  | Queen
  | Bishop

let format str = str |> String.trim |> String.lowercase_ascii

let char_in_range = function
  | 'a' .. 'h' -> true
  | _ -> false

let int_in_range = function
  | '1' .. '8' -> true
  | _ -> false

let pos_of_str str =
  let ch1 = String.get str 0 in
  let ch2 = String.get str 1 in
  if char_in_range ch1 && int_in_range ch2 && String.length str = 2 then
    (7 - (Char.code ch2 - 49), Char.code ch1 - 97)
  else raise Malformed

let rec get_words_no_spaces str_lst = List.filter (( <> ) "") str_lst

let parse str =
  let formatted_str = format str in
  let words_list = String.split_on_char ' ' formatted_str in
  match formatted_str with
  | "quit" -> Quit
  | "reset" -> Reset
  | "draw" -> Draw
  | "undo" -> Undo
  | "help" -> Help
  | "rules" -> Rules
  | "score" -> Score
  | _ ->
      if String.length formatted_str <= 1 then raise Malformed
      else if List.length words_list > 1 then
        let commands_no_spaces = get_words_no_spaces words_list in
        match commands_no_spaces with
        | [ a; b ] ->
            if String.length a <> 2 || String.length b <> 2 then raise Malformed
            else Move (pos_of_str a, pos_of_str b)
        | _ -> raise Malformed
      else raise Malformed

let promotion_parse str =
  let formatted_str = format str in
  match formatted_str with
  | "1"
  | "queen" ->
      Queen
  | "2"
  | "rook" ->
      Rook
  | "3"
  | "knight" ->
      Knight
  | "4"
  | "bishop" ->
      Bishop
  | _ -> raise Malformed
