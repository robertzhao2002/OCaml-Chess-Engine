exception EmptySquare
(** [EmptySquare] is raised when the player tries to move an empty square. *)

type t
(** The abstract type representing a chess piece. *)

type color = bool
(** The color of the piece. If true, then the piece is black, otherwise it is
    white. *)

val init_piece : string -> bool -> int -> int -> t
(** [init name color x y] initializes a piece with [name], [color], and position
    ([x], [y]). *)

val is_empty : t -> bool
(** [is_empty piece] is whether or not [piece] is of the Empty variant. *)

val position : t -> int * int
(** [position piece] is a tuple representing the position of [piece] on the
    board. *)

val value : t -> int
(** [value piece] is the scoring value of [piece]. The piece values are the same
    as official chess, with the exception of [King], whose value is set at
    1,000. *)

val name : t -> string
(** [name piece] is the unicode character that represents [piece]. *)

val is_king : t -> bool
(** [is_king piece] is whether or not [piece] is a [King]. *)

val is_pawn : t -> bool
(** [is_pawn piece] is whether or not [piece] is a [Pawn]. *)

val color : t -> bool
(** [color piece] is the color of [piece]. *)

val moves : t -> int
(** [moves piece] is the move counter of [piece]. *)

val valid_moves : t -> (int * int) list
(** [valid_moves piece] is a tuple list representing the possible next moves of
    [piece], based only on [piece]'s move logic. *)

val move_piece : int * int -> t -> t
(** [move_piece pos piece] is [piece] with its position updated to [pos]. *)
