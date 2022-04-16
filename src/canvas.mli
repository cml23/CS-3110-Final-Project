open Graphics

(**Representation of the game canvas.

   This module draws the board and pieces to the display, utilizing
   information about the graphics and locations associated with their
   respective data types.*)

val draw : State.t -> unit
(**[draw st] draws all elements associated with the current game state
   to the game canvas.*)

val mouse_input : Graphics.status -> Board.t -> (int * int) option
(**[mouse_input ev] is the [(x,y)] coordinate of the tile clicked by the
   user, or None if there is no tile at the position clicked. *)

val highlight : Graphics.status -> Board.t -> unit
(**[highlight ev b] highlights the piece clicked by the user in green. *)

val init : unit
