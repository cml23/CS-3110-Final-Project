type piece =
  | Some of {
      player : string;
      id : int;
      is_royal : bool;
    }
  | None

type tile = {
  x : int;
  (* maybe get rid of x,y and introduce functions to get x and y from
     the tile array *)
  y : int;
  pc : piece;
}

type t = tile array * int

let init_board : t = raise (Failure "Unimplemented: Board.init_board")
let from_json json = raise (Failure "Unimplemented: Board.from_json")

let pieces_of_player (board : t) (pl : string) : string list =
  raise (Failure "Unimplemented: Board.pieces_of_player")
(* function on a tile that returns None if no piece or Some of {player,
   id, is_royal} if a tile has a piece; function for the neighbors of
   tile; individual functions for getting each of 4 neighbors: return
   None if nothing there, or Some of (x,y) *)

(* (priority) for drawing: function for returning dimensions of board;
   (priority) function to provide list of tile info for every tile in
   board *)