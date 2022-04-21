open Graphics

(**Representation of the game canvas.

   This module draws the board and pieces to the display, utilizing
   information about the graphics and locations associated with their
   respective data types.*)

val swap_preset : int -> int
(**[swap_preset idx] increments the preset index by 1, if such a preset
   exists, or wraps around to 0 if this is not possible.*)

val init : unit
(**[init] initializes the Canvas and loads the images for all presets.*)

val draw : int -> State.t -> unit
(**[draw idx st] draws all elements associated with the preset numbered
   [idx] to the game canvas.*)

val mouse_input : Graphics.status -> Board.t -> (int * int) option
(**[mouse_input ev] is the [(x,y)] coordinate of the tile clicked by the
   user, or None if there is no tile at the position clicked. *)

val highlight : Graphics.status -> Board.t -> unit
(**[highlight ev b] highlights the piece clicked by the user in green. *)
