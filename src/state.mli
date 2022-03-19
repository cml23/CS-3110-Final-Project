(** Representation of dynamic game state.

    This module represents the current state of the board, modifies it
    based on moves and their legality, and checks whether a player has
    lost or not. *)

type t
(** The abstract value representing the game state, including a board,
    win state, winner, and a list of tiles that have been selected. *)

val init_state : Board.t -> t
(** [init_state Board.t] creates an initial game state based on
    [Board.t]. *)

val get_board : t -> Board.t
(** [get_board t] returns the board stored in state [t] for drawing. *)

val game_over : t -> bool
(** [game_over t] returns whether a player has won or not. *)

val get_victor : t -> string
(** [get_victor t] returns which player has won. *)

val unselected : t -> bool
(** [num_selected t] returns whether a tile has been selected. *)

val selected : t -> int * int
(** [num_selected t] returns whether a tile has been selected. *)

val check_victor : t -> t
(** [check_victor r] returns a new state that has determined whether a
    player has won or not.*)

(** [move] represents the type of state returned. Continue represents
    that the current player stays the same and requires new input. Legal
    represents that the board and player has changed. Illegal is similar
    to Continue but the player has inputted an invalid set of moves.*)
type move =
  | Continue of t
  | Legal of t
  | Illegal of t

val update : int * int -> t -> move
(** [update coord state] converts a coordinate into a selection or piece
    movement on the board. T is Continue if the first [coord] provided
    is contains a piece and Legal if the second [coord] provided is an
    empty square satisfying certain conditions. [move] is Illegal if the
    first [coord] provided does not contain a piece or the second
    [coord] provided is an illegal checkers move. *)
