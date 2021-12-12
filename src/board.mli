exception InvalidPos
(** [InvalidPos] is raised when attempting to access an invalid position on the
    board. *)

type t
(** The abstract type representing a board. *)

val init_board : t
(** [init_board] is a board configuration representing the initial state with
    all of the pieces placed in their correct initial positions. *)

val piece_at : t -> int * int -> Piece.t
(** [piece_at board pos] is the piece located on [board] at position [pos].
    Raises: [InvalidPos] if [pos] is not a valid position (either x or y is less
    than 0 or more than 7.) *)

val black_pieces : t -> Piece.t list
(** [black_pieces board] gets all the black pieces on [board].*)

val white_pieces : t -> Piece.t list
(** [white_pieces board] gets all the white pieces on [board].*)

val check : t -> bool
(** [check board] is whether [board] is in a 'check' state. A 'check' state
    happens when either side's [King] piece is being threatened by the opposing
    side's piece(s). *)

val next_moves : t -> Piece.t -> (int * int) list
(** [next_moves board piece] is a list of coordinates that represent legal
    positions that [piece] can move to on [board]. *)

val promotion : int * int -> t -> Piece.t -> t
(** [promotion pos board piece] is the pawn at position [pos] on [board]
    promoted to [piece]. *)

val move :
  int * int ->
  int * int ->
  ((int * int) * (int * int)) option ->
  t ->
  t * Piece.t
(** [move curr_pos new_pos en_passant board] is a pair of the board
    configuration after the piece on [curr_pos] in [board] is moved to
    [new_pos], and the piece that was just captured (Empty if nothing was
    captured). [en_passant] represents whether the board has a possible en
    passant to make, with [None] representing no possible en passant and
    [Some (your_pawn, enemy_pawn)] representing [your_pawn] can en passant
    [enemy_pawn]. Raises: [InvalidPos] if the attempted move is illegal.
    Examples of illegal moves include moving to a position not possible on the
    board, moving the other player's piece on your turn, moving a piece to
    somewhere it cannot move.*)

val to_string : t -> string
(** [to_string board] is a string representation of [board]. *)
