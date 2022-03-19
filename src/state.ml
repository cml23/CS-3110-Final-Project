open Board

exception RemoveNoPiece
exception NoPiece
exception NoMove

type t = {
  board : Board.t;
  game_over : bool;
  victor : string;
  selected : int * int;
  moves : (int * int) list;
  caps : (int * int) list * piece list;
  player_turn : int;
}
(* Should be [""] for no victor, ["player 1"], and ["player 2"] *)

let init_state (board : Board.t) : t =
  {
    board;
    game_over = false;
    victor = "";
    selected = (-1, -1);
    moves = [];
    caps = ([], []);
    player_turn = 1;
  }

let get_board (state : t) : Board.t = state.board
let game_over (state : t) : bool = state.game_over
let get_victor (state : t) : string = state.victor
let get_moves (state : t) : (int * int) list = state.moves
let get_caps (state : t) : (int * int) list * piece list = state.caps
let unselected (state : t) : bool = state.selected = (-1, -1)
let selected (state : t) : int * int = state.selected

let check_victor (state : t) : t =
  if Board.num_pcs_of_pl state.board 1 = 0 then
    { state with game_over = false; victor = "player 2" }
  else if Board.num_pcs_of_pl state.board 2 = 0 then
    { state with game_over = false; victor = "player 1" }
  else state

(** [get_xy_of_pc state pc] is a wrapper function for xy_of_pc in board.
    Returns the coordinates of [pc]. Postcondition: raises [NoPiece] if
    [pc] does not exist in the board.*)
let get_xy_of_pc (state : t) (pc : piece) : int * int =
  match xy_of_pc state.board pc with
  | Some c -> c
  | None -> raise NoPiece

(** [get_pc_of_xy state coord] is a wrapper function for pc_of_xy in
    board. Returns the piece at [coord]. Postcondition: raises [NoPiece]
    if [coord] does not contain a piece.*)
let get_pc_of_xy (state : t) (coord : int * int) : piece =
  match piece_of_xy state.board (fst coord) (snd coord) with
  | Some pc -> pc
  | None -> raise NoPiece

(** [get_idx caps coord acc] returns the index of [coord] in [caps].
    Precondition: [coord] is a valid second click.*)
let rec get_idx
    (caps : (int * int) list)
    (coord : int * int)
    (acc : int) : int =
  match caps with
  | [] -> raise NoMove
  | h :: t -> if h = coord then acc else get_idx t coord (acc + 1)

(** [get_cap_pc coord state] returns the piece to be captured associated
    with [coord]. Precondition: [coord] is a valid second click.*)
let get_cap_pc (coord : int * int) (state : t) : piece =
  try get_idx (fst state.caps) coord 0 |> List.nth (snd state.caps)
  with _ -> raise NoMove

(** [match_capture finish state] checks whether [finish] is is located
    in state.moves.*)
let match_move (finish : int * int) (state : t) : bool =
  List.exists (fun x -> x = finish) state.moves

(** [match_capture finish state] checks whether [finish] is is located
    in state.caps.*)
let match_capture (finish : int * int) (state : t) : bool =
  List.exists (fun x -> x = finish) (fst state.caps)

(** [move_pc start finish state] returns a board with a piece moved from
    [start] to [finish]. Precondition: [start] and [finish] are valid
    board coordinates and [start] contains a piece.*)
let move_pc (start : int * int) (finish : int * int) (state : t) :
    Board.t =
  let pc = get_pc_of_xy state start in
  let bd = add_pc state.board pc (fst finish) (snd finish) in
  del_pc bd (fst start) (snd start)

(** [move_cap_pc start finish state] returns a board with a piece moved
    from [start] to [finish] and with [rem_pc] removed. Precondition:
    [start] and [finish] are valid board coordinates, [start] contains a
    piece, and [rem_pc] exists in board.*)
let move_cap_pc
    (start : int * int)
    (finish : int * int)
    (rem_pc : piece)
    (state : t) : Board.t =
  let bd = move_pc start finish state in
  let cd = get_xy_of_pc state rem_pc in
  del_pc bd (fst cd) (snd cd)

(** [valid_fst_click coord state] checks whether or not [state] can
    store a coordinate (i.e. selected is empty) and whether the [coord]
    contains a piece in the board of [state]*)
let valid_fst_click (coord : int * int) (state : t) : bool =
  unselected state
  && pc_exists state.board (fst coord) (snd coord)
  && (get_pc_of_xy state coord).player = state.player_turn

(** [store_fst_click coord state] returns a state with selected
    containing [coord], moves containing legal moves from [coord], caps
    containing legal caps from [coord]. Precondition: [coord] in board
    contains a piece. *)
let store_fst_click (coord : int * int) (state : t) : t =
  let pc = get_pc_of_xy state coord in
  {
    state with
    selected = coord;
    moves = poss_moves state.board pc;
    caps = poss_captures state.board pc;
  }

(** [new_bd bd state] returns a state with board [bd] and the selected,
    moves, and caps fields cleared. Precondition: [bd] is the board
    after a legal move has occurred.*)
let new_bd (state : t) (bd : Board.t) : t =
  {
    state with
    board = bd;
    selected = (-1, -1);
    moves = [];
    caps = ([], []);
    player_turn = (if state.player_turn = 1 then 2 else 1);
  }

(** [move_state state coord] returns a state with the selected piece
    moved to [coord] on the board. *)
let move_state (state : t) (coord : int * int) : t =
  let bd = move_pc state.selected coord state in
  new_bd state bd

(** [move_cap_state state coord] returns a state with the selected piece
    moved to [coord] on the board and the intermittent piece captured. *)
let move_cap_state (state : t) (coord : int * int) : t =
  let pc = get_cap_pc coord state in
  let bd = move_cap_pc state.selected coord pc state in
  new_bd state bd

(** [move] represents the type of state returned. Continue represents
    that the current player stays the same and requires new input. Legal
    represents that the board and player has changed. Illegal is similar
    to Continue but the player has inputted an invalid set of moves.*)
type move =
  | Continue of t
  | Legal of t
  | Illegal of t

(** [update coord state] returns a move depending on the validity of
    [coord] and the curren [state] *)
let update (coord : int * int) (state : t) : move =
  if valid_fst_click coord state then
    Continue (store_fst_click coord state)
  else if not (unselected state) then
    if match_move coord state then Legal (move_state state coord)
    else if match_capture coord state then
      Legal (move_cap_state state coord)
    else Illegal state
  else Illegal state
