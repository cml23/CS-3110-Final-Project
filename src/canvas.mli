open Graphics

(**Representation of the game canvas.

   This module draws the board and pieces to the display, utilizing
   information about the graphics and locations associated with their
   respective data types.*)

val draw : State.t -> unit
(**[draw st] draws all elements associated with the current game state
   to the game canvas.*)

val mouse_input : Graphics.status -> unit
(**[mouse_input ev] handles the user mouse input based on information
   given by the status [ev]*)
