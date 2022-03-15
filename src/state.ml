open Board

type t = {
  board : Board.t;
  game_over : bool;
  victor : string;
  selected : (int * int) list;
  moves : (int * int) list;
  caps : (int * int) list * piece list;
}
(* Should be [""] for no victor, ["player 1"], and ["player 2"] *)

let init_state (board : Board.t) : t =
  {
    board;
    game_over = false;
    victor = "";
    selected = [];
    moves = [];
    caps = ([], []);
  }

let get_board (state : t) : Board.t = state.board
let game_over (state : t) : bool = state.game_over
let get_victor (state : t) : string = state.victor
let get_moves (state : t) : (int * int) list = state.moves
let get_caps (state : t) : (int * int) list * piece list = state.caps
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

let match_move (finish : int * int) (state : t) : bool =
  List.exists (fun x -> x = finish) state.moves

let match_capture (finish : int * int) (state : t) : bool =
  List.exists (fun x -> x = finish) (fst state.caps)

(** [move_pc start finish state] takes two valid board coordinates, the
    first one with a piece and the second without one.*)
let move_pc (start : int * int) (finish : int * int) (state : t) :
    Board.t =
  del_pc
    (add_pc state.board (piece_of_xy start) finish)
    (fst start) (snd start)

(* pos_moves *)
(* pos_captures *)
let update (coord : int * int) (state : t) =
  if
    num_selected state = 0
    && pc_exists state.board (fst coord) (snd coord)
  then
    Continue
      {
        state with
        selected = coord :: state.selected;
        moves =
          pos_moves (piece_of_xy state.board (fst coord) (snd coord));
        caps =
          pos_captures (piece_of_xy state.board (fst coord) (snd coord));
      }
  else if num_selected state = 1 then
    if match_move coord state then
      Legal
        {
          state with
          board = move_pc;
          selected = [];
          moves = [];
          caps = ([], []);
        }
    else Illegal state
  else Illegal state
