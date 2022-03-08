(**[tile_size] is a constant representing the x and y dimensions of a
   tile. *)
let tile_size = 40

(**[pc_size] is a constant representing the horizontal and vertical
   radius of a piece. *)
let pc_size = 40

(**[draw_piece pc x y] draws [pc] to the tile at [x] and [y].*)
let draw_piece (pc : Board.piece) (x : int) (y : int) : unit =
  let open Graphics in
  if pc.player = 1 then set_color red else set_color blue;
  fill_ellipse x y 40 40

(**[draw_tile x y tile_size color] draws a tile of [color] and size
   [tile_size] at position [(x,y)]*)
let draw_tile
    (x : int)
    (y : int)
    (tile_size : int)
    (color : Graphics.color) : unit =
  Graphics.set_color color;
  Graphics.fill_rect x y tile_size tile_size

(**[draw_row x y tile_size color cols] draws a row of [num] tiles
   colored with [color] starting at the position [(x,y). ] *)
let rec draw_row
    (x : int)
    (y : int)
    (tile_size : int)
    (color : Graphics.color)
    (num : int) : unit =
  draw_tile x y tile_size color;
  if num = 0 then ()
  else draw_row (x + tile_size) y tile_size color (num - 1)

(**[draw_board brd st] draws the board to the game canvas using the
   board configuration [bd].*)
let rec draw_board
    (*State.t*) (x : int)
    (y : int)
    (rows : int)
    (cols : int)
    (color : Graphics.color)
    (tile_size : int) : unit =
  draw_row x y tile_size color cols;
  if rows = 0 then ()
  else draw_board x (y - tile_size) color (rows - 1) cols tile_size

let draw st =
  let open Graphics in
  open_graph "";
  (*draw_board 0 (size_y () - (tile_size - 10)) 8 8 black 40;*)
  draw_piece { player = 2; id = 1; is_royal = false } 50 50;
  synchronize ();
  ignore (wait_next_event [ Key_pressed ])
