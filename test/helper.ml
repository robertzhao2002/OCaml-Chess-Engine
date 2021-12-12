open Chess.Board
open Chess.Piece
open Chess.State
open Chess.Command

(* Comparators *)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be {i set-like}, meaning that they do not contain any duplicates.
    Second, they must contain the same elements, though not necessarily in the
    same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(* Print Helpers *)
let id x = x

let position_printer (x, y) =
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let result_printer = function
  | Playing en_passant -> "Playing"
  | Promotion _ -> "Promotion"
  | WhiteWin -> "WhiteWin"
  | BlackWin -> "BlackWin"
  | Stalemate -> "Stalemate"

(* Board Moves *)
let rec move_times time (x, y) piece =
  match time with
  | 0 -> piece
  | _ -> move_times (time - 1) (x, y) (move_piece (x, y) piece)

let pos str1 =
  let ch1 = String.get str1 0 in
  let ch2 = String.get str1 1 in
  (7 - (Char.code ch2 - 49), Char.code ch1 - 97)

let move_helper str1 str2 board = move (pos str1) (pos str2) None board |> fst

let state_helper str1 str2 = change_state (pos str1) (pos str2)

let backrank color x =
  [
    init_piece "rook" color x 0;
    init_piece "knight" color x 1;
    init_piece "bishop" color x 2;
    init_piece "queen" color x 3;
    init_piece "king" color x 4;
    init_piece "bishop" color x 5;
    init_piece "knight" color x 6;
    init_piece "rook" color x 7;
  ]

let rec pawns color x y lst =
  if y >= 0 then pawns color x (y - 1) (init_piece "pawn" color x y :: lst)
  else lst
