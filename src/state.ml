open Board

type t = {
  board : Board.t;
  game_over : bool;
  victor : string;
  selected : (int * int) list;
}
(* Should be [""] for no victor, ["player 1"], and ["player 2"] *)

let init_state (board : Board.t) : t =
  { board; game_over = false; victor = ""; selected = [] }

let get_board (state : t) : Board.t = state.board
let game_over (state : t) : bool = state.game_over
let get_victor (state : t) : string = state.victor

let check_victor (state : t) : t =
  if Board.num_pcs_of_pl state.board 1 = 0 then
    { state with game_over = false; victor = "player 2" }
  else if Board.num_pcs_of_pl state.board 2 = 0 then
    { state with game_over = false; victor = "player 1" }
  else state

type move =
  | Continue of t
  | Legal of t
  | Illegal of t

let num_selected (state : t) : int = List.length state.selected

let update (coord : int * int) (state : t) =
  if
    num_selected state = 0
    &&
    match coord with
    | x, y -> (
        match piece_of_xy state.board x y with
        | Some piece -> true
        | None -> false)
  then Continue { state with selected = coord :: state.selected }
  else Illegal state
