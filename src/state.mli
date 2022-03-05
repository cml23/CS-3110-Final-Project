(** Representation of dynamic game state.

    This module represents the current state of the board, modifies it
    based on moves and their legality, and checks whether a player has
    lost or not. *)

type t
(** The abstract value representing the game state, including a board,
    win, and loss condition. *)

val init_state : Board.t -> t
(** [init_state Board.t] creates an initial game state based on
    [Board.t]. *)

val get_board : t -> Board.t
(** [get_board t] returns the board stored in state [t] for drawing. *)

val game_over : t -> bool
(** [game_over t] returns whether a player has won or not *)

val get_victor : t -> bool
(** [get_victor t] returns which player has won or not *)

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of t
  | Illegal

val go : string -> Adventure.t -> t -> result
(** [go exit adv st] is [r] if attempting to go through exit [exit] in
    state [st] and adventure [adv] results in [r]. If [exit] is an exit
    from the adventurer's current room, then [r] is [Legal st'], where
    in [st'] the adventurer is now located in the room to which [exit]
    leads. Otherwise, the result is [Illegal].

    Effects: none. [go] is not permitted to do any printing. *)
