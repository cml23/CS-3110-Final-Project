open Graphics

(**Representation of the game canvas.

   This module draws the board and pieces to the display, utilizing
   information about the graphics and locations associated with their
   respective data types.*)

val draw : State.t -> unit
(**[draw st] draws all elements associated with the current game state
   to the game canvas.*)
