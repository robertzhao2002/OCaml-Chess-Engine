exception Malformed
(** Raised when the inputed command is malformed. *)

type position = int * int
(** The type [position] represents a position on the board (x,y) where x and y
    are both integer in the range 0-7 inclusive. *)

(** The type [command] is a player's command which is then parsed into [Reset]
    for resetting the game, [Quit] for quitting the game, [Help] for accessing
    help menu, [Rules] for accessing all the commands again, [Undo] for undoing
    the last move, [Draw] to offer the opponent a draw, [Score] to check the
    current score, or [Move pos1 pos2], which represents the start and end
    position that a player wants to move a piece to. *)
type command =
  | Move of position * position
  | Reset
  | Quit
  | Undo
  | Help
  | Rules
  | Draw
  | Score

(** The type [promotion_pieces] represents all the possible pieces that can be
    promoted to. All the possible promotion pieces are [Knight], [Rook],
    [Queen], and [Bishop]. *)
type promotion_pieces =
  | Knight
  | Rook
  | Queen
  | Bishop

val parse : string -> command
(** [parse str] parses a player's input [str] into a [command]. A command is
    valid if it is not [Malformed]. See the examples below.

    Examples:

    - [parse "a3 c6"] is [Move (5,0),(2,2)]
    - [parse "rEsEt"] is [Reset].
    - [parse "Quit"] is [Quit].
    - [parse "Undo"] is [Undo].
    - [parse "hELp"] is [Help].
    - [parse "draW"] is [Draw].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Malformed] if [str] is malformed. An input string is malformed if
    it is empty after being trimmed or one of the valid commands. *)

val promotion_parse : string -> promotion_pieces
(** [promotion_parse str] is parses a promotion command [str] into one of the
    possible [promotion_pieces]. Raises: [Malformed] if [str] is malformed. An
    input string is malformed if it is not a valid piece to promote to or a
    string does not parse properly into a chess piece. *)
