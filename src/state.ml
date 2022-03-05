type t = {
  board : Board.t;
  win_state : bool;
  victor : bool;
}

let init_state (board : Board.t) : t =
  raise (Failure "Unimplemented: Board.from_json")

let get_board (state : t) : Board.t =
  raise (Failure "Unimplemented: Board.pieces_of_player")

let game_over : t -> bool =
  raise (Failure "Unimplemented: Board.pieces_of_player")

let get_victor : t -> bool =
  raise (Failure "Unimplemented: Board.pieces_of_player")

type result =
  | Legal of t
  | Illegal

let go : string -> Adventure.t -> t -> result =
  raise (Failure "Unimplemented: Board.pieces_of_player")
