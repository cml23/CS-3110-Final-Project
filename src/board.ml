type piece = {
  player : int;
  id : int;
  is_royal : bool;
}

type tile =
  | Some of piece
  | None

type t = tile array * int
(* a board can have any rectangular shape. The number of tiles per row
   (i.e., the number of columns) is given by the second number in the
   tuple representation. *)

let std_pc p i = Some { player = p; id = i; is_royal = false }

let init_board : t =
  ( [|
      std_pc 1 1;
      None;
      std_pc 1 2;
      None;
      std_pc 1 3;
      None;
      std_pc 1 4;
      None;
      None;
      std_pc 1 5;
      None;
      std_pc 1 6;
      None;
      std_pc 1 7;
      None;
      std_pc 1 8;
      std_pc 1 9;
      None;
      std_pc 1 10;
      None;
      std_pc 1 11;
      None;
      std_pc 1 12;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      std_pc 2 13;
      None;
      std_pc 2 14;
      None;
      std_pc 2 15;
      None;
      std_pc 2 16;
      std_pc 2 17;
      None;
      std_pc 2 18;
      None;
      std_pc 2 19;
      None;
      std_pc 2 20;
      None;
      None;
      std_pc 2 21;
      None;
      std_pc 2 22;
      None;
      std_pc 2 23;
      None;
      std_pc 2 24;
    |],
    8 )

let from_json json = raise (Failure "Unimplemented: Board.from_json")
let get_x (b : t) i = ((i - 1) mod snd b) + 1
let get_y (b : t) i = ((i - 1) / snd b) + 1
let get_idx (b : t) x y = (snd b * (y - 1)) + x
let dim_x (b : t) = snd b
let dim_y (b : t) = Array.length (fst b) / snd b

let piece_of_xy (b : t) x y =
  match Array.get (fst b) (get_idx b x y - 1) with
  | None -> (None : piece option)
  | Some pc -> (Some pc : piece option)

let pieces_of_player (board : t) (pl : string) : string list =
  raise (Failure "Unimplemented: Board.pieces_of_player")

(* TODO: functions for the neighbors of tile; individual functions for
   getting each of 4 neighbors: return None if nothing there, or Some of
   (x,y) *)
