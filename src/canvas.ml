(**[swap_color] changes the drawing color from red to black if the
   current color is black, and vice versa.*)
let swap_color (c : Graphics.color) : Graphics.color =
  if c = (Graphics.black : Graphics.color) then
    (Graphics.red : Graphics.color)
  else (Graphics.black : Graphics.color)

type preset = {
  (*p1_image : Images.t; p2_image : Images.t;*)
  image_paths : Images.t array;
  mutable tile1 : Graphics.image option;
  mutable tile2 : Graphics.image option;
  mutable soldier : Graphics.image option;
  mutable royal : Graphics.image option;
  mutable soldier2 : Graphics.image option;
  mutable royal2 : Graphics.image option;
  tiles : Graphics.image option array;
}

(**[default] is the default preset, with a black and white board. Preset
   images values are initially set to [None] since the graph has not yet
   been opened.*)
let default =
  {
    image_paths =
      [|
        Images.sub
          (Png.load_as_rgb24 "data/tile_black.png" [])
          0 0 Constants.tile_size Constants.tile_size;
        Images.sub
          (Png.load_as_rgb24 "data/tile_white.png" [])
          0 0 Constants.tile_size Constants.tile_size;
        Png.load_as_rgb24 "data/soldier_resize.png" [];
        Png.load_as_rgb24 "data/royal_resize.png" [];
        Png.load_as_rgb24 "data/soldier2_resize.png" [];
        Png.load_as_rgb24 "data/royal2_resize.png" [];
      |];
    tile1 = None;
    tile2 = None;
    soldier = None;
    royal = None;
    soldier2 = None;
    royal2 = None;
    tiles = [| None; None |];
  }

(**[load_images p] assigns the appropriate image stored in [p]'s
   [image_paths] to the fields of [p].*)
let load_images p =
  p.tile1 <- Some (Graphic_image.of_image p.image_paths.(0));
  p.tile2 <- Some (Graphic_image.of_image p.image_paths.(1));
  p.soldier <- Some (Graphic_image.of_image p.image_paths.(2));
  p.royal <- Some (Graphic_image.of_image p.image_paths.(3));
  p.soldier2 <- Some (Graphic_image.of_image p.image_paths.(4));
  p.royal2 <- Some (Graphic_image.of_image p.image_paths.(5));
  p.tiles.(0) <- p.tile1

(**[active_presets] are the presets that the player can choose from.*)
let active_presets = [| default |]

let turn1_img : Graphics.image option ref = ref None
let turn2_img : Graphics.image option ref = ref None

let swap_preset idx =
  if idx + 1 > Array.length active_presets - 1 then 0 else idx + 1

let swap_index (tile_index : int) = if tile_index = 0 then 1 else 0

(**[draw_piece_img img x y] draws the specified image to the appropriate
   part of the canvas.*)
let draw_img (img : Graphics.image option) (x : int) (y : int) =
  match img with
  | None -> ()
  | Some image -> Graphics.draw_image image x y

(**[draw_piece x y b pc tile_size] draws [pc] to the tile at [x] and
   [y].*)
let draw_piece
    (x : int)
    (y : int)
    (b : Board.t)
    (pc : Board.piece)
    (tile_size : int)
    (p : preset) : unit =
  let open Graphics in
  if pc.player = 1 then
    if pc.is_royal then
      draw_img p.royal
        (x + (Constants.pc_size / 2))
        (y + (Constants.pc_size / 2))
    else
      draw_img p.soldier
        (x + (Constants.pc_size / 2))
        (y + (Constants.pc_size / 2))
  else if pc.is_royal then
    draw_img p.royal2
      (x + (Constants.pc_size / 2))
      (y + (Constants.pc_size / 2))
  else
    draw_img p.soldier2
      (x + (Constants.pc_size / 2))
      (y + (Constants.pc_size / 2))
(*fill_ellipse (x + (tile_size / 2)) (y + (tile_size / 2))
  Constants.pc_size Constants.pc_size;*)
(* match Board.xy_of_pc b pc with | Some (x, y) -> fill_ellipse x y 40
   40 | None -> ()*)

(**[draw_tile x y row col b tile_size color] draws the tile [(row,col)]
   of [color] and size [tile_size] at position [(x,y)]*)
let rec draw_tile
    (x : int)
    (y : int)
    (row : int)
    (col : int)
    (b : Board.t)
    (tile_size : int)
    (p : preset)
    (tile_index : int) : unit =
  match p.tiles.(tile_index) with
  | None -> ()
  | Some v -> (
      Graphics.draw_image v x y;
      match Board.piece_of_xy b row col with
      | Some pc -> draw_piece x y b pc tile_size p
      | None -> ())

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
    (p : preset)
    (tile_index : int) : unit =
  draw_tile x y row col b tile_size p tile_index;
  if i = 1 then ()
  else
    draw_row (x + tile_size) y (row + 1) col b (i - 1) tile_size p
      (swap_index tile_index)

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
    (p : preset)
    tile_index : unit =
  draw_row x y row col b (Board.dim_x b) tile_size p tile_index;
  if y_dim = 1 then ()
  else
    draw_board x (y + tile_size) row (col + 1) b (y_dim - 1) tile_size p
      (swap_index tile_index)

(**[get_coordinate i dim_start] calculates the coordinate of a tile from
   its pixel location on the screen.*)
let get_coordinate i dim_start =
  (float_of_int i -. float_of_int dim_start)
  /. float_of_int Constants.tile_size
  |> Float.floor |> ( +. ) 1. |> int_of_float

(**[find_tile x y start_x start_y] is the tile at [(x,y)] on the board
   where the bottom left hand corner of tile (1,1) is at position
   [(start_x, start_y)]*)
let rec find_tile x y start_x start_y b =
  if
    x > start_x + (Board.dim_x b * Constants.tile_size)
    || y > start_y + (Board.dim_y b * Constants.tile_size)
    || x < start_x || y < start_y
  then None
  else
    let tile_x = get_coordinate x start_x in
    let tile_y = get_coordinate y start_y in
    Some
      ( tile_x,
        tile_y,
        start_x + ((tile_x - 1) * Constants.tile_size),
        start_y + ((tile_y - 1) * Constants.tile_size) )

let mouse_input (ev : Graphics.status) (b : Board.t) :
    (int * int) option =
  match
    find_tile ev.mouse_x ev.mouse_y Constants.start_x Constants.start_y
      b
  with
  | Some (x, y, _, _) -> Some (x, y)
  | _ -> None

let highlight (ev : Graphics.status) (b : Board.t) =
  match
    find_tile ev.mouse_x ev.mouse_y Constants.start_x Constants.start_y
      b
  with
  | None -> ()
  | Some (tile_x, tile_y, draw_x, draw_y) ->
      draw_tile draw_x draw_y tile_x tile_y b Constants.tile_size
        default 0
(*need green tile*)

(**[load_turn_img img1 img2] loads [img1] as the player 1 turn text, and
   [img2] as the player 2 turn text.*)
let load_turn_img img1 img2 =
  turn1_img := Some (Graphic_image.of_image (Png.load_as_rgb24 img1 []));
  turn2_img := Some (Graphic_image.of_image (Png.load_as_rgb24 img2 []))

let draw_turn_img (player : int) x y =
  if player = 1 then draw_img !turn1_img x y
  else draw_img !turn2_img x y

let init =
  Graphics.open_graph "";
  (*Images must be loaded after opening graph to avoid errors.*)
  load_turn_img "data/player1.png" "data/player2.png";
  for i = 0 to Array.length active_presets - 1 do
    load_images active_presets.(i)
  done

let draw i st =
  let b = State.get_board st in
  draw_board Constants.start_x Constants.start_y 1 1 b (Board.dim_y b)
    Constants.tile_size active_presets.(i) 0;
  draw_turn_img (State.get_player st)
    (Constants.start_x + (Board.dim_x b * (Constants.tile_size + 1)))
    Constants.start_y
