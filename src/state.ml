open Board
open Stack

exception RemoveNoPiece
exception NoPiece

exception NoMove
(** Exceptions for defensive programming to statisfy functino
    postconditions. *)

type move = {
  start : int * int;
  finish : int * int;
  cap_sq : int * int;
  cap_pc : piece option;
}
(** Stores information about past moves, will be grouped in a stack. *)

type t = {
  board : Board.t;
  game_over : bool;
  victor : string;
  sel : int * int;
  sel_pc : piece option;
  moves : (int * int) list;
  caps : (int * int) list * piece list;
  player_turn : int;
  mc_pres : bool;
  mc_pc : piece option;
  redos : move Stack.t;
  undos : move Stack.t;
}
(* Should be [""] for no victor, ["player 1"], and ["player 2"] *)

let init_state (board : Board.t) : t =
  {
    board;
    game_over = false;
    victor = "";
    sel = (-1, -1);
    sel_pc = None;
    moves = [];
    caps = ([], []);
    player_turn = 1;
    mc_pres = false;
    mc_pc = None;
    redos = Stack.create ();
    undos = Stack.create ();
  }

(*=========GETTER FUNCTIONS=========*)

let get_board (state : t) : Board.t = state.board
let game_over (state : t) : bool = state.game_over
let get_victor (state : t) : string = state.victor
let get_player (state : t) : int = state.player_turn
let get_moves (state : t) : (int * int) list = state.moves
let get_caps (state : t) : (int * int) list = fst state.caps
let unselected (state : t) : bool = state.sel = (-1, -1)
let selected (state : t) : int * int = state.sel
let get_if_mc (state : t) : bool = state.mc_pres

let get_undos (state : t) : move list =
  state.undos |> Stack.to_seq |> List.of_seq

(*=========INDEX FUNCTIONS=========*)

let bmatcher = function
  | Some b -> true
  | None -> false

(** [matcher a] factors out matching code for piece options. Returns the
    piece in [a] if it exists and throws [NoPiece] otherwise.*)
let matcher = function
  | Some b -> b
  | None -> raise NoPiece

(** [get_xy_of_pc state pc] is a wrapper function for xy_of_pc in board.
    Returns the coordinates of [pc]. Postcondition: raises [NoPiece] if
    [pc] does not exist in the board.*)
let get_xy_of_pc (pc : piece) (bd : Board.t) : int * int =
  matcher (xy_of_pc bd pc)

(** [get_pc_of_xy state coord] is a wrapper function for pc_of_xy in
    board. Returns the piece at [coord]. Postcondition: raises [NoPiece]
    if [coord] does not contain a piece.*)
let get_pc_of_xy (coord : int * int) (bd : Board.t) : piece =
  matcher (piece_of_xy bd (fst coord) (snd coord))

(** [get_idx caps coord acc] returns the index of [coord] in [caps].
    Precondition: [coord] is a valid second click. Raises [NoMove] if
    [caps] is empty or [coord] does not exist in [caps].*)
let rec get_idx
    (caps : (int * int) list)
    (coord : int * int)
    (acc : int) : int =
  match caps with
  | [] -> raise NoMove
  | h :: t -> if h = coord then acc else get_idx t coord (acc + 1)

(** [get_cap_pc coord state] returns the piece to be captured associated
    with [coord]. Precondition: [coord] is a valid second click. Raises
    [NoMove] if no capturable piece is found. *)
let get_cap_pc
    (coord : int * int)
    (caps : (int * int) list * piece list) : piece =
  get_idx (fst caps) coord 0 |> List.nth (snd caps)

(** [match_capture finish state] checks whether [finish] is is located
    in state.caps.*)
let match_coord (finish : int * int) (coords : (int * int) list) : bool
    =
  List.exists (fun x -> x = finish) coords

(** [valid_reclick coord state] checks whether or not the [coord]
    contains a piece in the board of [state] that matches the player
    turn.*)
let valid_reclk (coord : int * int) (state : t) : bool =
  pc_exists state.board (fst coord) (snd coord)
  && (get_pc_of_xy coord state.board).player = state.player_turn

(** [valid_fst_click coord state] checks whether or not [state] can
    store a coordinate (i.e. selected is empty) and whether the [coord]
    contains a piece in the board of [state].*)
let valid_fst_clk (coord : int * int) (state : t) : bool =
  unselected state && valid_reclk coord state

(*=========BOARD MANIPULATION FUNCTIONS=========*)

(** [move_pc start finish state] returns a board with a piece moved from
    [start] to [finish]. Precondition: [start] and [finish] are valid
    board coordinates and [start] contains a piece.*)
let move_pc (pc : piece) (finish : int * int) (bd : Board.t) : Board.t =
  let start = get_xy_of_pc pc bd in
  let nbd = add_pc bd pc (fst finish) (snd finish) in
  del_pc nbd (fst start) (snd start)

(** [cap_pc pms board] returns a board that carries the first move in
    [pms] removed. Precondition:[pms] is not empty and its last move is
    a capture.*)
let cap_pc (mv : move) (bd : Board.t) : Board.t =
  del_pc bd (fst mv.cap_sq) (snd mv.cap_sq)

let uncap_pc (mv : move) (bd : Board.t) : Board.t =
  add_pc bd (matcher mv.cap_pc) (fst mv.cap_sq) (snd mv.cap_sq)

(*=========STATE MANIPULATION FUNCTIONS=========*)

(** [store_fst_clk coord state] returns a state with selected containing
    [coord], moves containing legal moves from [coord], caps containing
    legal caps from [coord]. Precondition: [coord] is the first click
    and contains a piece.*)
let store_fst_clk (coord : int * int) (state : t) : t =
  let pc = get_pc_of_xy coord state.board in
  {
    state with
    sel = coord;
    sel_pc = Some pc;
    moves = poss_moves state.board pc;
    caps = poss_captures state.board pc;
  }

(** [add_move state pm] returns a state with [pm] prepended to
    past_moves. Precondition: [pm] is a valid move.*)
let add_mv (mv : move) (state : t) : t =
  Stack.push mv state.undos;
  state

(** [store_move coord state] adds a move or capture to [state] and
    returns it. Precondition: [coord] is a valid second click.*)
let create_mv (move : bool) (coord : int * int) (state : t) : move =
  if move then
    {
      start = state.sel;
      finish = coord;
      cap_sq = (-1, -1);
      cap_pc = None;
    }
  else
    let cpc = get_cap_pc coord state.caps in
    {
      start = state.sel;
      finish = coord;
      cap_sq = get_xy_of_pc cpc state.board;
      cap_pc = Some cpc;
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
    sel = (-1, -1);
    sel_pc = None;
    moves = [];
    caps = ([], []);
    player_turn = (if state.player_turn = 1 then 2 else 1);
  }

(** [movecap_st move pc coord state] returns a state with the selected
    piece moved to [coord] on the board and the intermittent piece
    captured if it is a capture. *)
let mvcap_st (move : bool) (pc : piece) (finish : int * int) (state : t)
    : t =
  move_pc pc finish state.board
  |> (if move then Fun.id else cap_pc (Stack.top state.undos))
  |> new_bd state

(** [check_mc state coord] checks whether the piece that has moved to
    [coord] can capture again without promotion and stores the
    information in state. Precondition: [coord] contains a piece. *)
let check_mc (pc : piece) (state : t) : t =
  let caps = poss_captures state.board pc in
  if List.length (fst caps) > 0 then
    {
      state with
      player_turn = pc.player;
      caps;
      mc_pres = true;
      mc_pc = Some pc;
    }
  else { state with mc_pres = false; mc_pc = None }

(** [check_vc] updates [state] if either of the players have no more
    pieces.*)
let check_vc (state : t) : t =
  if Board.num_pcs_of_pl state.board 1 = 0 then
    { state with game_over = false; victor = "player 2" }
  else if Board.num_pcs_of_pl state.board 2 = 0 then
    { state with game_over = false; victor = "player 1" }
  else state

(** [pro_pc state coord] attempts to promote a piece that has moved to
    [coord] and returns a new state accordingly. Should occur after
    [check_mc] to prevent post promotion captures.*)
let pro_pc (pc : piece) (coord : int * int) (state : t) : t =
  if is_promotable state.board pc then
    promote_pc state.board pc |> new_bd state
  else state

(** [pipeline move mc coord state] changes [state] depending on the
    legal second click [coord], whether it is a [move], and whether it
    is undergoing multicapture. Precondition: Either a move or capture
    is possible. [coord] is valid move or capture location of selected
    piece. *)
let pipeline
    (is_mv : bool)
    (mv : move)
    (pc : piece)
    (finish : int * int)
    (state : t) : t =
  state |> add_mv mv
  |> mvcap_st is_mv pc finish
  |> reset_st
  |> (if is_mv then Fun.id else check_mc pc)
  |> (if is_mv then Fun.id else check_vc)
  |> pro_pc pc finish

let reg_move (is_mv : bool) (finish : int * int) (state : t) : t =
  let pc = matcher state.sel_pc in
  let mv = create_mv is_mv finish state in
  pipeline is_mv mv pc finish state

let uncap_st (u : move) (state : t) : t =
  state.board |> uncap_pc u |> new_bd state

(** Relies heavily on preconditions. Precondition: [state] has a move to
    undo.*)
let undo_move (state : t) : t =
  let u = Stack.pop state.undos in
  Stack.push u state.redos;
  (if u.cap_pc |> bmatcher then uncap_st u state else state)
  |> mvcap_st true (get_pc_of_xy u.finish state.board) u.start
  |> reset_st

let redo_move (state : t) : t =
  let r = Stack.pop state.redos in
  let is_mv = not (bmatcher r.cap_pc) in
  let pc = get_pc_of_xy r.start state.board in
  pipeline is_mv r pc r.finish state

(*=========UPDATE AND IMMEDIATE HFs=========*)

(** [turn] represents the type of state returned. Continue represents
    that the current player stays the same and requires new input. Legal
    represents that the board and player has changed. Illegal is similar
    to Continue but the player has inputted an invalid set of moves.*)
type turn =
  | Continue of t
  | Legal of t
  | Illegal of t

(** [legal_action coord state] returns whether second click [coord] is
    located within the possible moves or captures of [state].*)
let legal_act (coord : int * int) (state : t) : bool =
  match_coord coord state.moves || match_coord coord (fst state.caps)

(** [match_mc_pc f coord state] returns whether [coord] and [state]
    statisfy function [f] and that [coord] contains the piece undergoing
    multicapture. Precondition: [f] checks whether [coord] has a piece
    for short circuit evaluation.*)
let match_mc_pc f (coord : int * int) (state : t) : bool =
  f coord state && Some (get_pc_of_xy coord state.board) = state.mc_pc

(** [legal_mc coord state] handles move legality when there is a forced
    multicapture for one player depending on [coord] and [state].*)
let legal_mc (coord : int * int) (state : t) : turn =
  if match_mc_pc valid_fst_clk coord state then
    Continue (store_fst_clk coord state)
  else if (not (unselected state)) && match_coord coord (fst state.caps)
  then Legal (reg_move false coord state)
  else if match_mc_pc valid_reclk coord state then
    Continue (store_fst_clk coord state)
  else Illegal state

(** [update coord state] returns a turn depending on the validity of
    [coord] and the current [state]. *)
let update (state : t) (coord : int * int) : turn =
  if state.mc_pres then legal_mc coord state
  else if valid_fst_clk coord state then
    Continue (store_fst_clk coord state)
  else if (not (unselected state)) && legal_act coord state then begin
    if Stack.length state.redos > 0 then Stack.clear state.redos;
    let legal_move = match_coord coord state.moves in
    Legal (reg_move legal_move coord state)
  end
  else if valid_reclk coord state then
    Continue (store_fst_clk coord state)
  else Illegal state

(*=========UNDO AND REDO AND HFs=========*)

let check_do (f : t -> t) (stack : move Stack.t) (state : t) : turn =
  if Stack.length stack < 1 then Illegal state else Legal (f state)

let urdo (undo : bool) (state : t) : turn =
  if undo then check_do undo_move state.undos state
  else check_do redo_move state.redos state
