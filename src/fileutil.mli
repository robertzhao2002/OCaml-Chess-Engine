exception NotFound
(** [NotFound] is raised when a file name could not be found in the data
    directory. *)

exception InvalidText of int
(** [InvalidText line] is raised when a file contains text on [line] that cannot
    be parsed as a valid chess move. *)

exception InvalidPos of int
(** [InvalidPos line] is raised when a text file causes [Board.InvalidPos] to be
    raised on [line]. *)

exception WrongColor of int
(** [WrongColor line] is raised when a text file causes [State.WrongColor] to be
    raised on [line]. *)

exception InvalidPromotion of int
(** [InvalidPromotion line] is raised when a text file causes
    [State.IllegalPromotion] to be raised on [line]. *)

val config : string -> State.t
(** [read_file file] is the state that is produced by parsing each line of the
    file [file]. Raises: [NotFound] if [file] cannot be found;
    [InvalidText line] if a [line] in [file] cannot be parsed; [InvalidPos line]
    if a [line] in [file] makes an invalid move; [WrongColor line] if a [line]
    in [file] moves on the wrong color's turn; [IllegalPromotion line] if a
    [line] in [file] tries to promote illegally. *)
