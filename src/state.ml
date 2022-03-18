open Board

exception RemoveNoPiece
exception NoPiece
exception NoMove

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

let get_pc_xy (pc : piece) (state : t) : int * int =
  match xy_of_pc state.board pc with
  | Some c -> c
  | None -> raise NoPiece

let get_xy_pc (coord : int * int) (state : t) : piece =
  match piece_of_xy state.board (fst coord) (snd coord) with
  | Some pc -> pc
  | None -> raise NoPiece

let rec get_idx
    (coord : int * int)
    (caps : (int * int) list)
    (acc : int) : int =
  match caps with
  | [] -> raise NoMove
  | h :: t -> if h = coord then acc else get_idx coord t (acc + 1)

let get_cap_pc (coord : int * int) (state : t) : piece =
  try get_idx coord (fst state.caps) 0 |> List.nth (snd state.caps)
  with _ -> raise NoMove

let match_move (finish : int * int) (state : t) : bool =
  List.exists (fun x -> x = finish) state.moves

let match_capture (finish : int * int) (state : t) : bool =
  List.exists (fun x -> x = finish) (fst state.caps)

(** [move_pc start finish state] takes two valid board coordinates, the
    first one with a piece and the second without one, and moves the
    piece.*)
let move_pc (start : int * int) (finish : int * int) (state : t) :
    Board.t =
  let pc = get_xy_pc start state in
  let bd = add_pc state.board pc (fst finish) (snd finish) in
  del_pc bd (fst start) (snd start)

(** [move_pc start finish state] takes two valid board coordinates, the
    first one with a piece and the second without one.*)
let move_cap_pc
    (start : int * int)
    (finish : int * int)
    (rem_pc : piece)
    (state : t) : Board.t =
  let bd = move_pc start finish state in
  let cd = get_pc_xy rem_pc state in
  del_pc bd (fst cd) (snd cd)

let new_bd (bd : Board.t) (state : t) : t =
  { state with board = bd; selected = []; moves = []; caps = ([], []) }

let update (coord : int * int) (state : t) =
  if
    num_selected state = 0
    && pc_exists state.board (fst coord) (snd coord)
  then
    let pc = get_xy_pc coord state in
    Continue
      {
        state with
        selected = coord :: state.selected;
        moves = poss_moves state.board pc;
        caps = poss_captures state.board pc;
      }
  else if num_selected state = 1 then
    let start = List.nth state.selected 0 in
    let finish = coord in
    if match_move coord state then
      let bd = move_pc start finish state in
      Legal (new_bd bd state)
    else if match_capture coord state then
      let pc = get_cap_pc coord state in
      let bd = move_cap_pc start finish pc state in
      Legal (new_bd bd state)
    else Illegal state
  else Illegal state
