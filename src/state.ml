open Board

exception RemoveNoPiece
exception NoPiece
exception NoMove

type multi_capture = {
  present : bool;
  piece : piece option;
  caps : (int * int) list * piece list;
}

type t = {
  board : Board.t;
  game_over : bool;
  victor : string;
  selected : int * int;
  moves : (int * int) list;
  caps : (int * int) list * piece list;
  player_turn : int;
  mc : multi_capture;
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
    mc = { present = false; piece = None; caps = ([], []) };
  }

let get_board (state : t) : Board.t = state.board
let game_over (state : t) : bool = state.game_over
let get_victor (state : t) : string = state.victor
let get_player (state : t) : int = state.player_turn
let get_moves (state : t) : (int * int) list = state.moves
let get_caps (state : t) : (int * int) list = fst state.caps
let unselected (state : t) : bool = state.selected = (-1, -1)
let selected (state : t) : int * int = state.selected
let get_if_mc (state : t) : bool = state.mc.present
let get_mc_caps (state : t) : (int * int) list = fst state.mc.caps

let check_victor (state : t) : t =
  if Board.num_pcs_of_pl state.board 1 = 0 then
    { state with game_over = false; victor = "player 2" }
  else if Board.num_pcs_of_pl state.board 2 = 0 then
    { state with game_over = false; victor = "player 1" }
  else state

(** [get_xy_of_pc state pc] is a wrapper function for xy_of_pc in board.
    Returns the coordinates of [pc]. Postcondition: raises [NoPiece] if
    [pc] does not exist in the board.*)
let get_xy_of_pc (pc : piece) (board : Board.t) : int * int =
  match xy_of_pc board pc with
  | Some c -> c
  | None -> raise NoPiece

(** [get_pc_of_xy state coord] is a wrapper function for pc_of_xy in
    board. Returns the piece at [coord]. Postcondition: raises [NoPiece]
    if [coord] does not contain a piece.*)
let get_pc_of_xy (coord : int * int) (board : Board.t) : piece =
  match piece_of_xy board (fst coord) (snd coord) with
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
let get_cap_pc
    (coord : int * int)
    (caps : (int * int) list * piece list) : piece =
  try get_idx (fst caps) coord 0 |> List.nth (snd caps)
  with _ -> raise NoMove

(** [match_capture finish state] checks whether [finish] is is located
    in state.caps.*)
let match_coord (finish : int * int) (coords : (int * int) list) : bool
    =
  List.exists (fun x -> x = finish) coords

(** [move_pc start finish state] returns a board with a piece moved from
    [start] to [finish]. Precondition: [start] and [finish] are valid
    board coordinates and [start] contains a piece.*)
let move_pc (start : int * int) (finish : int * int) (board : Board.t) :
    Board.t =
  let pc = get_pc_of_xy start board in
  let bd = add_pc board pc (fst finish) (snd finish) in
  del_pc bd (fst start) (snd start)

(** [move_cap_pc rem_pc board] returns a board with [rem_pc] removed.
    Precondition:[rem_pc] exists in board.*)
let cap_pc (rem_pc : piece) (board : Board.t) : Board.t =
  let cd = get_xy_of_pc rem_pc board in
  del_pc board (fst cd) (snd cd)

(** [valid_reclick coord state] checks whether or not the [coord]
    contains a piece in the board of [state] that matches the player
    turn.*)
let valid_reclick (coord : int * int) (state : t) : bool =
  pc_exists state.board (fst coord) (snd coord)
  && (get_pc_of_xy coord state.board).player = state.player_turn

(** [valid_fst_click coord state] checks whether or not [state] can
    store a coordinate (i.e. selected is empty) and whether the [coord]
    contains a piece in the board of [state].*)
let valid_fst_click (coord : int * int) (state : t) : bool =
  unselected state && valid_reclick coord state

(** [store_fst_click coord state] returns a state with selected
    containing [coord], moves containing legal moves from [coord], caps
    containing legal caps from [coord]. Precondition: [coord] is the
    first click and contains a piece.*)
let store_fst_click (coord : int * int) (state : t) : t =
  let pc = get_pc_of_xy coord state.board in
  {
    state with
    selected = coord;
    moves = poss_moves state.board pc;
    caps = poss_captures state.board pc;
  }

(** [new_bd bd state] returns a state with board [bd] and the selected,
    moves, and caps fields cleared. Precondition: [bd] is the board
    after a legal move has occurred.*)
let new_bd (state : t) (bd : Board.t) : t = { state with board = bd }

(** [reset_st state] returns a state with the selected, moves, and caps
    fields cleared. Precondition: a move has just occurred. *)
let reset_st (state : t) : t =
  {
    state with
    selected = (-1, -1);
    moves = [];
    caps = ([], []);
    player_turn = (if state.player_turn = 1 then 2 else 1);
  }

(** [move_state state coord] returns a state with the selected piece
    moved to [coord] on the board. *)
let move_state (coord : int * int) (state : t) : t =
  move_pc state.selected coord state.board |> new_bd state

(** [move_cap_state state coord] returns a state with the selected piece
    moved to [coord] on the board and the intermittent piece captured. *)
let move_cap_state
    (coord : int * int)
    (caps : (int * int) list * piece list)
    (state : t) : t =
  let pc = get_cap_pc coord caps in
  move_pc state.selected coord state.board |> cap_pc pc |> new_bd state

(** [check_mc state coord] checks whether the piece that has moved to
    [coord] can capture again without promotion and stores the
    information in state. Precondition: [coord] contains a piece. *)
let check_mc (coord : int * int) (state : t) : t =
  let pc = get_pc_of_xy coord state.board in
  let caps = poss_captures state.board pc in
  if List.length (fst caps) > 0 then
    {
      state with
      player_turn = pc.player;
      mc = { present = true; piece = Some pc; caps };
    }
  else
    {
      state with
      mc = { present = false; piece = None; caps = ([], []) };
    }

(** [pro_pc state coord] attempts to promote a piece that has moved to
    [coord] and returns a new state accordingly. Should occur after
    [check_mc] to prevent post promotion captures.*)
let pro_pc (coord : int * int) (state : t) : t =
  let pc = get_pc_of_xy coord state.board in
  if is_promotable state.board pc then
    promote_pc state.board pc |> new_bd state
  else state

(** [legal_action coord state] returns whether second click [coord] is
    located within the possible moves or captures of [state].*)
let legal_action (coord : int * int) (state : t) : bool =
  match_coord coord state.moves || match_coord coord (fst state.caps)

(** [pipeline move mc coord state] changes [state] depending on the
    legal second click [coord], whether it is a [move], and whether it
    is undergoing multicapture. Precondition: Either a move or capture
    is possible. [coord] is valid move or capture location of selected
    piece. *)
let pipeline (move : bool) (mc : bool) (coord : int * int) (state : t) :
    t =
  if move then move_state coord state |> reset_st |> pro_pc coord
  else
    move_cap_state coord
      (if mc then state.mc.caps else state.caps)
      state
    |> reset_st |> check_mc coord |> pro_pc coord

(** [move] represents the type of state returned. Continue represents
    that the current player stays the same and requires new input. Legal
    represents that the board and player has changed. Illegal is similar
    to Continue but the player has inputted an invalid set of moves.*)
type move =
  | Continue of t
  | Legal of t
  | Illegal of t

(** [match_mc_pc f coord state] returns whether [coord] and [state]
    statisfy function [f] and that [coord] contains the piece undergoing
    multicapture. Precondition: [f] checks whether [coord] has a piece
    for short circuit evaluation.*)
let match_mc_pc f (coord : int * int) (state : t) : bool =
  f coord state
  && Some (get_pc_of_xy coord state.board) = state.mc.piece

(** [legal_mc coord state] handles move legality when there is a forced
    multicapture for one player depending on [coord] and [state].*)
let legal_mc (coord : int * int) (state : t) : move =
  if match_mc_pc valid_fst_click coord state then
    Continue (store_fst_click coord state)
  else if
    (not (unselected state)) && match_coord coord (fst state.mc.caps)
  then Legal (pipeline false true coord state)
  else if match_mc_pc valid_reclick coord state then
    Continue (store_fst_click coord state)
  else Illegal state

(** [update coord state] returns a move depending on the validity of
    [coord] and the current [state]. *)
let update (state : t) (coord : int * int) : move =
  if state.mc.present then legal_mc coord state
  else if valid_fst_click coord state then
    Continue (store_fst_click coord state)
  else if (not (unselected state)) && legal_action coord state then
    let legal_move = match_coord coord state.moves in
    Legal (pipeline legal_move false coord state)
  else if valid_reclick coord state then
    Continue (store_fst_click coord state)
  else Illegal state
