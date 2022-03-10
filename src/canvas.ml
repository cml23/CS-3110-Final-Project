(*[swap_color] changes the drawing color from red to black if the
  current color is black, and vice versa.*)
let swap_color (c : Graphics.color) : Graphics.color =
  if c = (Graphics.black : Graphics.color) then
    (Graphics.red : Graphics.color)
  else (Graphics.black : Graphics.color)

(**[draw_piece pc x y] draws [pc] to the tile at [x] and [y].*)
let draw_piece (b : Board.t) (pc : Board.piece) : unit =
  let open Graphics in
  if pc.player = 1 then set_color (red : Graphics.color)
  else set_color (blue : Graphics.color);
  match Board.xy_of_pc b pc with
  | Some (x, y) -> fill_ellipse x y 40 40
  | None -> ()

(**[draw_tile x y tile_size color] draws a tile of [color] and size
   [tile_size] at position [(x,y)]*)
let rec draw_tile
    (x : int)
    (y : int)
    (i : int)
    (tile_size : int)
    (color : Graphics.color) : unit =
  Graphics.moveto x y;
  Graphics.set_color (color : Graphics.color);
  Graphics.fill_rect x y tile_size tile_size

(**[draw_row x y tile_size color cols] draws a row of [num] tiles
   colored with [color] starting at the position [(x,y). ] *)
let rec draw_row
    (x : int)
    (y : int)
    (b : Board.t)
    (i : int)
    (tile_size : int)
    (color : Graphics.color) : unit =
  draw_tile x y (Board.dim_x b) tile_size color;
  if i = 1 then ()
  else
    draw_row (x + tile_size) y b (i - 1) tile_size
      (swap_color (color : Graphics.color))

(**[draw_board brd st] draws the board to the game canvas using the
   board configuration [bd].*)
let rec draw_board
    (x : int)
    (y : int)
    (b : Board.t)
    (y_dim : int)
    (tile_size : int)
    (color : Graphics.color) : unit =
  draw_row x y b (Board.dim_x b) tile_size color;
  Graphics.moveto x y;
  if y_dim = 1 then ()
  else
    draw_board x (y + tile_size) b (y_dim - 1) tile_size
      (swap_color (color : Graphics.color))

let draw st =
  Graphics.open_graph "";
  let b = Board.init_board in
  draw_board 50 50 b (Board.dim_y b) 40
    (Graphics.black : Graphics.color)
(*draw_tile 0 0 (Board.init_board |> Board.dim_x) 40 black*)
(*draw_piece Board.init_board { player = 2; id = 1; is_royal = false*)
