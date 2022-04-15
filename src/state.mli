open Board
(** Representation of dynamic game state.

    This module represents the current state of the board, modifies it
    based on moves and their legality, and checks whether a player has
    lost or not. *)

type move = {
  start : int * int;
  finish : int * int;
  pc : piece;
  cap_sq : int * int;
  cap_pc : piece option;
  mc_pres : bool;
}
(** Public type move that stores a snapshot of abstract type t that
    allows for undoing moves. *)

type t
(** The abstract value representing the game state, including a board,
    win state, winner, undos, and redos. *)

val init_state : int -> Board.t -> t
(** [init_state init bd] creates an initial game state based on
    [Board.t]. *)

val def_state : t
(** [def_state] creates the default checkers board. *)

val get_board : t -> Board.t
(** [get_board t] returns the board stored in state [t] for drawing. *)

val get_player : t -> int
(** [get_player t] returns the player of the current turn. 1 or 2.*)

val game_over : t -> bool
(** [game_over t] returns whether a player has won or not. *)

val get_vc : t -> int
(** [get_victor t] returns which player has won. *)

val get_pts : int -> t -> int
(** [get_pts player t] returns the number of points the player with id
    [player] has gotten. Should be called when [gameover] is [true].*)

val get_moves : t -> (int * int) list
(** [get_moves t] returns which coordinates the selected piece can move
    to. *)

val get_caps : t -> (int * int) list
(** [get_moves t] returns which coordinates the selected piece can move
    to while capturing. *)

val unselected : t -> bool
(** [num_selected t] returns whether a tile has been selected. *)

val selected : t -> int * int
(** [num_selected t] returns whether a tile has been selected. *)

val get_if_mc : t -> bool
(** [get_if_mc t] returns whether the state is in multicapture mode or
    not. *)

val get_undos : t -> move list
(** [get_undoes state] returns the possible undos for the current state.
    For testing purposes. *)

val get_redos : t -> move list
(** [get_redos state] returns the possible redos for the current state.
    For testing purposes. *)

(** [turn] represents the type of state returned. Continue represents
    that the current player stays the same and requires new input. Legal
    represents that the board and player has changed. Illegal is similar
    to Continue but the player has inputted an invalid set of moves.*)
type turn =
  | Continue of t
  | Legal of t
  | Illegal of t
  | NoUndo of t
  | NoRedo of t

val update : int * int -> t -> turn
(** [update coord state] converts a coordinate into a selection or piece
    movement on the board. [turn] is Continue if the first [coord]
    provided is contains a piece and Legal if the second [coord]
    provided is an empty square satisfying certain conditions. [turn] is
    Illegal if the first [coord] provided does not contain a piece or
    the second [coord] provided is an illegal checkers move. *)

val urdo : bool -> t -> turn
(** [urdo undo state] undos [state] by a move if undo is [true] and
    redos a move otherwise. Will always return *)

val match_turn :
  (t -> 'a) ->
  (t -> 'a) ->
  (t -> 'a) ->
  (t -> 'a) ->
  (t -> 'a) ->
  turn ->
  'a

val get_state : turn -> t