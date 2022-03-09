type t = {
  board : Board.t;
  win_state : bool;
  victor : string;
  selected : (int * int) list;
}
(* Should be [""] for no victor, ["player 1"], and ["player 2"] *)

let init_state (board : Board.t) : t =
  { board; win_state = false; victor = ""; selected = [] }

let get_board (state : t) : Board.t = state.board
let game_over (state : t) : bool = state.win_state
let get_victor (state : t) : string = state.victor

type move =
  | Continue of t
  | Legal of t
  | Illegal

let num_selected (state : t) : int = List.length state.selected

let update (coord : int * int) (state : t) =
  raise (Failure "Unimplemented: Board.pieces_of_player")
