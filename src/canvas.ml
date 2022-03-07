(**[draw_piece tile_id] draws a piece to the specified tile.*)
let draw_piece (tile_id : int) : unit =
  failwith "Unimplemented: draw_piece"

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
  else draw_board x (y + tile_size) color (rows - 1) cols tile_size

let draw st =
  Graphics.open_graph "";
  draw_board 0 0 8 8 Graphics.black 40
