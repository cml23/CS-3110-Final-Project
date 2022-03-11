open Board

type t = {
  board : Board.t;
  game_over : bool;
  victor : string;
  selected : (int * int) list;
  legals : (int * int * piece option) list;
}
(* Should be [""] for no victor, ["player 1"], and ["player 2"] *)

let init_state (board : Board.t) : t =
  { board; game_over = false; victor = ""; selected = []; legals = [] }

let get_board (state : t) : Board.t = state.board
let game_over (state : t) : bool = state.game_over
let get_victor (state : t) : string = state.victor

let get_legals (state : t) : (int * int * piece option) list =
  state.legals

let num_selected (state : t) : int = List.length state.selected
let selected (state : t) : (int * int) list = state.selected

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

let pc_exist (state : t) (coord : int * int) : bool =
  match coord with
  | x, y -> (
      match piece_of_xy state.board x y with
      | Some piece -> true
      | None -> false)

let nb_match nbf (start : int * int) (finish : int * int) : bool =
  match start with
  | x, y -> (
      match nbf x y with
      | Some final -> final = finish
      | None -> false)

let calculate_legals (state : t) (start : int * int) : (int * int) list
    =
  []

let update (coord : int * int) (state : t) =
  if num_selected state = 0 && pc_exist state coord then
    Continue { state with selected = coord :: state.selected }
  else if num_selected state = 1 then
    if nb_match (up_l state.board) (List.nth state.selected 0) coord
    then Continue state
    else Illegal state
  else Illegal state
