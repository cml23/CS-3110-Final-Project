(**[draw_piece pc x y] draws [pc] to the tile at [x] and [y].*)
let draw_piece (b : Board.t) (pc : Board.piece) : unit =
  let open Graphics in
  if pc.player = 1 then set_color red else set_color blue;
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
  Graphics.fill_rect x y tile_size tile_size;
  if i = 0 then ()
  else draw_tile (x + tile_size) y (i - 1) tile_size color

(**[draw_row x y tile_size color cols] draws a row of [num] tiles
   colored with [color] starting at the position [(x,y). ] *)
let rec draw_row
    (x : int)
    (y : int)
    (b : Board.t)
    (tile_size : int)
    (color : Graphics.color) : unit =
  for i = 1 to Board.dim_x b do
    draw_tile (x + tile_size) y (Board.dim_x b) tile_size color
  done

(**[draw_board brd st] draws the board to the game canvas using the
   board configuration [bd].*)
let rec draw_board
    (x : int)
    (y : int)
    (b : Board.t)
    (y_dim : int)
    (color : Graphics.color)
    (tile_size : int) : unit =
  draw_tile x y (Board.dim_x b) tile_size color;
  if y_dim = 0 then ()
  else draw_board x (y + tile_size) b (y_dim - 1) tile_size color

let draw st =
  let open Graphics in
  open_graph "";
  let b = Board.init_board in
  draw_board 50 50 b (Board.dim_y b) black 40
(*draw_tile 0 0 (Board.init_board |> Board.dim_x) 40 black*)
(*draw_piece Board.init_board { player = 2; id = 1; is_royal = false*)
