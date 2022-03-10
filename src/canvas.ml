(**[swap_color] changes the drawing color from red to black if the
   current color is black, and vice versa.*)
let swap_color (c : Graphics.color) : Graphics.color =
  if c = (Graphics.black : Graphics.color) then
    (Graphics.red : Graphics.color)
  else (Graphics.black : Graphics.color)

(**[draw_piece x y b pc tile_size] draws [pc] to the tile at [x] and
   [y].*)
let draw_piece
    (x : int)
    (y : int)
    (b : Board.t)
    (pc : Board.piece)
    (tile_size : int) : unit =
  let open Graphics in
  if pc.player = 1 then set_color (yellow : Graphics.color)
  else set_color (cyan : Graphics.color);
  fill_ellipse
    (x + (tile_size / 2))
    (y + (tile_size / 2))
    Constants.pc_size Constants.pc_size
(* match Board.xy_of_pc b pc with | Some (x, y) -> fill_ellipse x y 40
   40 | None -> ()*)

(**[draw_tile x y row col b tile_size color] draws the tile
   [(row,
   col)] of [color] and size [tile_size] at position [(x,y)]*)
let rec draw_tile
    (x : int)
    (y : int)
    (row : int)
    (col : int)
    (b : Board.t)
    (tile_size : int)
    (color : Graphics.color) : unit =
  Graphics.moveto x y;
  Graphics.set_color (color : Graphics.color);
  Graphics.fill_rect x y tile_size tile_size;
  match Board.piece_of_xy b row col with
  | Some pc -> draw_piece x y b pc tile_size
  | None -> ()

(**[draw_row x y row col b i tile_size color cols] draws a row of [i]
   tiles colored with [color] starting at the position [(x,y). ] *)
let rec draw_row
    (x : int)
    (y : int)
    (row : int)
    (col : int)
    (b : Board.t)
    (i : int)
    (tile_size : int)
    (color : Graphics.color) : unit =
  draw_tile x y row col b tile_size color;
  if i = 1 then ()
  else
    draw_row (x + tile_size) y (row + 1) col b (i - 1) tile_size
      (swap_color (color : Graphics.color))

(**[draw_board x y row col b y_dim tile_size color] draws the board to
   the game canvas using the board configuration of [b].*)
let rec draw_board
    (x : int)
    (y : int)
    (row : int)
    (col : int)
    (b : Board.t)
    (y_dim : int)
    (tile_size : int)
    (color : Graphics.color) : unit =
  draw_row x y row col b (Board.dim_x b) tile_size color;
  Graphics.moveto x y;
  if y_dim = 1 then ()
  else
    draw_board x (y + tile_size) row (col + 1) b (y_dim - 1) tile_size
      (swap_color (color : Graphics.color))

let draw st =
  Graphics.open_graph "";
  let b = Board.init_board in
  draw_board 130 80 1 1 b (Board.dim_y b) Constants.tile_size
    (Graphics.black : Graphics.color)
(*draw_tile 0 0 (Board.init_board |> Board.dim_x) 40 black*)
(*draw_piece Board.init_board { player = 2; id = 1; is_royal = false*)
