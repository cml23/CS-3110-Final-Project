type piece =
  | Some of {
      player : string;
      id : int;
      is_royal : bool;
    }
  | None

type tile = {
  x : int;
  y : int;
  pc : piece;
}

type t = tile array array

let from_json json = raise (Failure "Unimplemented: Board.from_json")

let pieces_of_player (board : t) (pl : string) : string list =
  raise (Failure "Unimplemented: Board.pieces_of_player")