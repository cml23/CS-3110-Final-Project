(**Representation of the game canvas.

   This module draws the board and pieces to the display, utilizing
   information about the graphics and locations associated with their
   respective data types.*)

val swap_preset : int -> int
(**[swap_preset idx] increments the preset index by 1, if such a preset
   exists, or wraps around to 0 if this is not possible.*)

val player_names : string list
(**[player_names] are the possible names that the players can choose
   from.*)

val p1_name : string ref
(**[p1_name] is the name selected by Player 1.*)

val p2_name : string ref
(**[p2_name] is the name selected by Player 2.*)

val init : Board.t -> unit
(**[init b] initializes the Canvas with board [b] and loads the images
   for all presets.*)

val draw : int -> State.t -> unit
(**[draw idx st] draws all elements associated with the preset numbered
   [idx] and state [st] to the game canvas.*)

val draw_new_game : int -> int -> int -> State.t -> unit
(**[draw preset p1_score p2_score st] draws a new game, with an updated
   score for player 1 and player 2, if the players have played multiple
   games.*)

val mouse_input : Graphics.status -> Board.t -> (int * int) option
(**[mouse_input ev b] is the [(x,y)] coordinate specified by the event
   handler [ev] of the tile clicked by the user on Board [b], or None if
   there is no tile at the position clicked. *)

val highlight : Graphics.status -> Board.t -> unit
(**[highlight ev b] highlights the piece clicked by the user in green. *)
