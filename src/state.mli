(** The type representing the current mode of a state. [Playing en_passant] is
    the normal mode, where [en_passant] represents whether an en_passant move is
    possible. [Promotion pos] is a promotion mode where the pawn on [pos] can
    promote. [WhiteWin] and [BlackWin] represent a win for the white or black
    side respectively, resulting from a checkmate. [Stalemate] represents a
    stalemate, where one side cannot make a move but is also not in checkmate.*)
type result =
  | Playing of ((int * int) * (int * int)) option
  | Promotion of (int * int)
  | WhiteWin
  | BlackWin
  | Stalemate

exception WrongColor
(** [WrongColor] is raised when a player tries moving the wrong color pieces. *)

exception NoUndo
(** [NoUndo] is raised when a player tries undoing to a state that does not
    exist. *)

exception IllegalPromotion
(** [IllegalPromotion] is raised when a player tries to promote when there is no
    possible promotion, or promotes to an invalid piece. *)

type t
(** The abstract type representing a state of the game. *)

val init_state : t
(** [init_state] is the initial state of the game. *)

val turn : t -> bool
(** [turn state] is the current turn of [state].*)

val board : t -> Board.t
(** [board state] is the current board of [state].*)

val result : t -> result
(** [result state] is the result of [state]. *)

val graveyard : t -> bool -> Piece.t list
(** [graveyard state color] is the current graveyard, or all the captured
    pieces, of [color] player in [state]. *)

val score : t -> bool -> int
(** [score state color] is the current score of the player [color] in [state]. *)

val checkmate : t -> bool
(** [checkmate state] is whether or not [state] is in a 'checkmate' state. A
    'checkmate' state happens when either side's [King] piece is being
    threatened by the opposing side's piece(s) and has no possible moves to
    avoid the threat. *)

val stalemate : t -> bool
(** [stalemate state] is whether or not [state] is in a 'stalemate' state. *)

val undo : t -> t
(** [undo state] goes back to the previous state of [state]. *)

val promotion_piece : Piece.t -> t -> t
(**[promotion_piece piece state] is the new state from promoting a pawn to
   [piece] in [state]. *)

val change_state : int * int -> int * int -> t -> t
(** [change_state curr_pos new_pos state] is the new state after moving a piece
    at [curr_pos] to [new_pos] on the board of [state]. Raises: [InvalidPos] if
    the attempted change is invalid; [WrongColor] if the color of the piece
    being moved does not match [state]'s turn. *)
