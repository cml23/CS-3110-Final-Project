(**[swap_color] changes the drawing color from red to black if the
   current color is black, and vice versa.*)
let swap_color (c : Graphics.color) : Graphics.color =
  if c = (Graphics.black : Graphics.color) then
    (Graphics.red : Graphics.color)
  else (Graphics.black : Graphics.color)

type preset = {
  image_paths : Images.t array;
  mutable tile1 : Graphics.image option;
  mutable tile2 : Graphics.image option;
  mutable soldier : Graphics.image option;
  mutable royal : Graphics.image option;
  mutable soldier2 : Graphics.image option;
  mutable royal2 : Graphics.image option;
  mutable highlight : Graphics.image option;
  tiles : Graphics.image option array;
}
(**[preset] contains all the skins for tiles, pieces, and highlighted
   tiles associated with a particular graphical preset selected by the
   player.*)

(**[preset_skeleton] is the skeleton on which all presets are based,
   with fields initialized to None so as to avoid image loading errors.*)
let preset_skeleton =
  {
    image_paths = Array.make 7 (Png.load_as_rgb24 "data/tile1.png" []);
    tile1 = None;
    tile2 = None;
    soldier = None;
    royal = None;
    soldier2 = None;
    royal2 = None;
    highlight = None;
    tiles = Array.make 3 None;
  }

(**[load_image path] loads the image stored at [path].*)
let load_image path = Png.load_as_rgb24 path []

(**[construct_preset file1 file2] creates a preset using the soldier,
   tile, and royal files labeled with the ints [file1] and [file2].*)
let construct_preset (file1 : int) (file2 : int) =
  {
    preset_skeleton with
    image_paths =
      [|
        Images.sub
          (load_image ("data/tile" ^ string_of_int file1 ^ ".png"))
          0 0 Constants.tile_size Constants.tile_size;
        Images.sub
          (load_image ("data/tile" ^ string_of_int file2 ^ ".png"))
          0 0 Constants.tile_size Constants.tile_size;
        load_image ("data/soldier" ^ string_of_int file1 ^ "_resize.png");
        load_image ("data/royal" ^ string_of_int file1 ^ "_resize.png");
        load_image ("data/soldier" ^ string_of_int file2 ^ "_resize.png");
        load_image ("data/royal" ^ string_of_int file2 ^ "_resize.png");
        Images.sub
          (load_image "data/highlight.png")
          0 0 Constants.tile_size Constants.tile_size;
      |];
    tiles = [| None; None; None |];
  }

(**[default] is the default preset, with a black and white board. Preset
   images values are initially set to [None] since the graph has not yet
   been opened.*)
let default = construct_preset 1 2

(**[yellow_purple] is a preset with a yellow and purple board and
   pieces.*)
let yellow_purple = construct_preset 3 4

(**[orange_blue] is a preset with a yellow and purple board and pieces.*)
let orange_blue = construct_preset 5 6

(**[black_red] is a preset with a black and red board and pieces.*)
let black_red = construct_preset 8 7

(**[make_transparent pc] converts the background of a piece image [pc]
   from black to transparent.*)
let make_transparent pc =
  match pc with
  | Some img ->
      let image = Graphics.dump_image img in
      for i = 0 to Array.length image - 1 do
        let row = image.(i) in
        for j = 0 to Array.length row - 1 do
          if row.(j) = 0 then row.(j) <- Graphics.transp else ()
        done
      done;
      Some (Graphics.make_image image)
  | None -> pc

(**[load_images p] assigns the appropriate image stored in [p]'s
   [image_paths] to the fields of [p].*)
let load_images p =
  p.tile1 <- Some (Graphic_image.of_image p.image_paths.(0));
  p.tile2 <- Some (Graphic_image.of_image p.image_paths.(1));
  p.soldier <-
    Some (Graphic_image.of_image p.image_paths.(2)) |> make_transparent;
  p.royal <-
    Some (Graphic_image.of_image p.image_paths.(3)) |> make_transparent;
  p.soldier2 <-
    Some (Graphic_image.of_image p.image_paths.(4)) |> make_transparent;
  p.royal2 <-
    Some (Graphic_image.of_image p.image_paths.(5)) |> make_transparent;
  p.highlight <- Some (Graphic_image.of_image p.image_paths.(6));
  p.tiles.(0) <- p.tile1;
  p.tiles.(1) <- p.tile2;
  p.tiles.(2) <- p.highlight

(**[active_presets] are the presets that the player can choose from.*)
let active_presets =
  [| default; yellow_purple; orange_blue; black_red |]

(**[turn1_img] is the image of the name associated with Player 1.*)
let turn1_img : Graphics.image option ref = ref None

(**[turn2_img] is the image of the name associated with Player 2.*)
let turn2_img : Graphics.image option ref = ref None

let swap_preset idx =
  if idx + 1 > Array.length active_presets - 1 then 0 else idx + 1

(**[swap_index] swaps the [tile_index] to that of the next tile that
   will be drawn.*)
let swap_index (tile_index : int) = if tile_index = 0 then 1 else 0

(**[current_preset] represents the preset from [active_presets] that is
   currently displayed to the board. It is by default 0, i.e. the
   [default] preset.*)
let current_preset = ref 0

(**[draw_img img x y] draws the specified image to the appropriate part
   of the canvas.*)
let draw_img (img : Graphics.image option) (x : int) (y : int) =
  match img with
  | None -> ()
  | Some image -> Graphics.draw_image image x y

(**[draw_piece x y b pc tile_size] draws [pc], whose image is drawn from
   preset [p], to the tile at canvas position [(x,y)]*)
let draw_piece
    (x : int)
    (y : int)
    (b : Board.t)
    (pc : Board.piece)
    (tile_size : int)
    (p : preset) : unit =
  let open Graphics in
  let draw_pos = Constants.pc_size / 2 in
  if pc.player = 1 then
    if pc.is_royal then draw_img p.royal (x + draw_pos) (y + draw_pos)
    else draw_img p.soldier (x + draw_pos) (y + draw_pos)
  else if pc.is_royal then
    draw_img p.royal2 (x + draw_pos) (y + draw_pos)
  else draw_img p.soldier2 (x + draw_pos) (y + draw_pos)

(**[draw_tile x y row col b tile_size color] draws the tile of board [b]
   at board index [(row,col)] with the image from the preset [p]'s
   [p.tiles] at index [tile_index] and size [tile_size] at position
   [(x,y)]*)
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

(**[draw_row x y row col b i tile_size color cols] draws a row of the
   board [b] with [i] tiles. The current tile in the row being drawn is
   located at tile index ([row], [col]), and the image that is assigned
   to this particular tile is found in preset [p]'s [p.tiles] at the
   index [tile_index]. All tiles are drawn at dimensions [tile_size] and
   are drawn on the canvas at [(x, y)].*)
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

(**[draw_board x y row col b y_dim tile_size color] draws the board [b],
   which has [y_dim] number of rows left to be drawn to the baord. The
   next tile to be drawn is located at board position [(row, col)], will
   be drawn at canvas position [(x, y)] with size [tile_size], and will
   take its image from the tile indexed at [tile_index] in [p.tiles].*)
let rec draw_board
    (x : int)
    (y : int)
    (row : int)
    (col : int)
    (b : Board.t)
    (y_dim : int)
    (tile_size : int)
    (p : preset)
    (tile_index : int) : unit =
  draw_row x y row col b (Board.dim_x b) tile_size p tile_index;
  if y_dim = 1 then ()
  else
    draw_board x (y + tile_size) row (col + 1) b (y_dim - 1) tile_size p
      (swap_index tile_index)

(**[get_coordinate i dim_start] calculates the coordinate of a tile
   based on its pixel location on the screen.*)
let get_coordinate i dim_start =
  (float_of_int i -. float_of_int dim_start)
  /. float_of_int Constants.tile_size
  |> Float.floor |> ( +. ) 1. |> int_of_float

(**[start_dim] is the [(x,y)] position where the board should begin
   being drawn to the Canvas.*)
let start_dim graphics_dim board_dim b =
  (graphics_dim () - (board_dim b * Constants.tile_size)) / 2

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
    find_tile ev.mouse_x ev.mouse_y
      (start_dim Graphics.size_x Board.dim_x b)
      (start_dim Graphics.size_y Board.dim_y b)
      b
  with
  | Some (x, y, _, _) -> Some (x, y)
  | _ -> None

let highlight (ev : Graphics.status) (b : Board.t) =
  match
    find_tile ev.mouse_x ev.mouse_y
      (start_dim Graphics.size_x Board.dim_x b)
      (start_dim Graphics.size_y Board.dim_y b)
      b
  with
  | None -> ()
  | Some (tile_x, tile_y, draw_x, draw_y) ->
      draw_tile draw_x draw_y tile_x tile_y b Constants.tile_size
        active_presets.(!current_preset)
        2

let player_names = [ "Howard"; "Cassidy"; "Anirudh"; "AI" ]
let p1_name = ref "Howard"
let p2_name = ref "Anirudh"

(**[load_turn_img] loads the turn image [img] based on the name selected
   by the player in [Main].*)
let load_turn_img img =
  Some
    (Graphic_image.of_image
       (Png.load_as_rgb24
          ("data/" ^ String.lowercase_ascii img ^ ".png")
          []))

let draw_turn_img (player : int) x y =
  Graphics.moveto x y;
  Graphics.set_color Graphics.white;
  Graphics.fill_rect x y 50 13;
  if player = 1 then draw_img !turn1_img x y
  else draw_img !turn2_img x y

(**[draw_score p1_score p2_score x y] draws the multi-game score to the
   canvas.*)
let draw_score p1_score p2_score x y =
  Graphics.moveto x y;
  Graphics.set_color Graphics.black;
  Graphics.draw_string
    (string_of_int p1_score ^ " : " ^ string_of_int p2_score)

let init b =
  Graphics.open_graph "";
  let size_x =
    (Board.dim_x b * Constants.tile_size) + Constants.start_x + 100
  in
  let size_y =
    (Board.dim_y b * Constants.tile_size) + Constants.start_y + 100
  in
  Graphics.resize_window size_x size_y;
  (*Images must be loaded after opening graph to avoid errors.*)
  for i = 0 to Array.length active_presets - 1 do
    load_images active_presets.(i)
  done

let draw i st =
  (*turn images must be loaded separately to ensure they are updated
    based on player input*)
  turn1_img := load_turn_img !p1_name;
  turn2_img := load_turn_img !p2_name;
  current_preset := i;
  let b = State.get_board st in
  let start_x = start_dim Graphics.size_x Board.dim_x b in
  let start_y = start_dim Graphics.size_y Board.dim_y b in
  draw_turn_img (State.get_player st)
    (start_x + (Board.dim_x b * (Constants.tile_size + 1)))
    start_y;
  draw_board start_x start_y 1 1 b (Board.dim_y b) Constants.tile_size
    active_presets.(!current_preset)
    0

let draw_new_game i p1_score p2_score st =
  let b = State.get_board st in
  init b;
  Graphics.clear_graph ();
  let start_y = start_dim Graphics.size_y Board.dim_y b in
  draw_score p1_score p2_score
    (Graphics.size_x () / 2)
    (start_y + (Board.dim_y b * Constants.tile_size) + 10);
  draw i st
