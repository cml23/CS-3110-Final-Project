open Board
open Stack

exception NoPiece
(** [NoPiece] represents whenever the board is indexed in such a way
    where [None] is return when a piece was expected.*)

exception NoCap
(** [NoCap] represents when a retrieval attempt to find the valid
    capture square fails.*)

(* TODO: Add multicapture and promotion information. *)
type move = {
  start : int * int;
  finish : int * int;
  pc : piece;
  cap_sq : int * int;
  cap_pc : piece option;
  mc_pres : bool;
}
(** Grouped in a stack. *)

type t = {
  board : Board.t;
  game_over : bool;
  victor : int;
  sel : int * int;
  sel_pc : piece option;
  moves : (int * int) list;
  caps : (int * int) list * piece list;
  player_turn : int;
  mc_pres : bool;
  redos : move Stack.t;
  undos : move Stack.t;
}
(** t stores temporary information as the player clicks around. Only
    when the player makes a legal move will information be committed to
    a move in the undos pile. Hence the at first glance redundancy.*)

let init_state (pl : int) (bd : Board.t) =
  {
    board = bd;
    game_over = false;
    victor = 0;
    sel = (-1, -1);
    sel_pc = None;
    moves = [];
    caps = ([], []);
    player_turn = pl;
    mc_pres = false;
    redos = Stack.create ();
    undos = Stack.create ();
  }

let def_state : t = init_state 1 Board.init_board

(*=========GETTER FUNCTIONS=========*)

let get_board (state : t) : Board.t = state.board
let game_over (state : t) : bool = state.game_over
let get_vc (state : t) : int = state.victor

let get_pts (player : int) (state : t) : int =
  if player = state.victor then 1 else 0

let get_player (state : t) : int = state.player_turn
let get_moves (state : t) : (int * int) list = state.moves
let get_caps (state : t) : (int * int) list = fst state.caps
let unselected (state : t) : bool = state.sel = (-1, -1)
let selected (state : t) : int * int = state.sel
let get_if_mc (state : t) : bool = state.mc_pres

let get_undos (state : t) : move list =
  state.undos |> Stack.to_seq |> List.of_seq

let get_redos (state : t) : move list =
  state.redos |> Stack.to_seq |> List.of_seq

(*=========INDEX FUNCTIONS=========*)

(** [bmatcher a]*)
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
  | [] -> raise NoCap
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
let move_pc (reverse : bool) (mv : move) (bd : Board.t) : Board.t =
  let start = if reverse then mv.finish else mv.start in
  let finish = if reverse then mv.start else mv.finish in
  let nbd = add_pc bd mv.pc (fst finish) (snd finish) in
  del_pc nbd (fst start) (snd start)

(** [cap_pc pms board] returns a board that carries the first move in
    [pms] removed. Precondition:[pms] is not empty and its last move is
    a capture.*)
let cap_pc (mv : move) (bd : Board.t) : Board.t =
  del_pc bd (fst mv.cap_sq) (snd mv.cap_sq)

(** [uncap_pc mv bd]*)
let uncap_pc (mv : move) (bd : Board.t) : Board.t =
  add_pc bd (matcher mv.cap_pc) (fst mv.cap_sq) (snd mv.cap_sq)

(*=========LOW LEVEL STATE MANIPULATION FUNCTIONS=========*)

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

(** [store_move coord state] adds a move or capture to [state] and
    returns it. Precondition: [coord] is a valid second click.*)
let create_mv (move : bool) (coord : int * int) (state : t) : move =
  if move then
    {
      start = state.sel;
      finish = coord;
      pc = matcher state.sel_pc;
      cap_sq = (-1, -1);
      cap_pc = None;
      mc_pres = false;
    }
  else
    let cpc = get_cap_pc coord state.caps in
    {
      start = state.sel;
      finish = coord;
      pc = matcher state.sel_pc;
      cap_sq = get_xy_of_pc cpc state.board;
      cap_pc = Some cpc;
      mc_pres = state.mc_pres;
    }

(** [add_move state pm] returns a state with [pm] prepended to
    past_moves. Precondition: [pm] is a valid move.*)
let add_mv (mv : move) (state : t) : t =
  Stack.push mv state.undos;
  state

(** [new_bd bd state] returns a state with board [bd] and the selected,
    moves, and caps fields cleared. Precondition: [bd] is the board
    after a legal move has occurred.*)
let new_bd (state : t) (bd : Board.t) : t = { state with board = bd }

let set_tn (pl : int) (state : t) : t = { state with player_turn = pl }

(** [mvcap_st move pc coord state] returns a state with the selected
    piece moved to [coord] on the board and the intermittent piece
    captured if it is a capture. NOTE: Also used by [undo_move], should
    not reference [undos] stack for non captures.*)
let mv_st (reverse : bool) (mv : move) (state : t) : t =
  move_pc reverse mv state.board |> new_bd state

let cap_st (mv : move) (state : t) : t =
  cap_pc mv state.board |> new_bd state

(** [reset_st state] returns a state with the selected, moves, and caps
    fields cleared. Precondition: a move has just occurred. *)
let reset_st (state : t) : t =
  {
    state with
    sel = (-1, -1);
    sel_pc = None;
    moves = [];
    caps = ([], []);
  }

let switch_tn (state : t) : t =
  if state.player_turn = 1 then set_tn 2 state else set_tn 1 state

(** [check_mc state coord] checks whether the piece that has moved to
    [coord] can capture again without promotion and stores the
    information in state. Precondition: [coord] contains a piece. *)
let check_mc (reverse : bool) (mv : move) (state : t) : t =
  let pc = mv.pc in
  let caps = poss_captures state.board pc in
  if List.length (fst caps) > 0 then
    {
      state with
      player_turn = pc.player;
      sel = (if reverse then mv.start else mv.finish);
      sel_pc = Some pc;
      caps;
      mc_pres = true;
    }
  else { state with mc_pres = false }

(** [pro_pc state coord] attempts to promote a piece that has moved to
    [coord] and returns a new state accordingly. Should occur after
    [check_mc] to prevent post promotion captures.*)
let pro_pc (mv : move) (state : t) : t =
  let pc = mv.pc in
  if is_promotable state.board pc then
    promote_pc state.board pc |> new_bd state
  else state

(** [check_vc] updates [state] if either of the players have no more
    pieces.*)
let check_vc (state : t) : t =
  if Board.num_pcs_of_pl state.board 1 = 0 then
    { state with game_over = true; victor = 2 }
  else if Board.num_pcs_of_pl state.board 2 = 0 then
    { state with game_over = true; victor = 1 }
  else state

(** [uncap_st u state] restores the piece captured by [u] to [state] and
    returns it. *)
let uncap_st (u : move) (state : t) : t =
  state.board |> uncap_pc u |> new_bd state

(* [rem_mc state] *)
let rem_mc (state : t) : t = { state with mc_pres = false }

(*=========HIGH LEVEL STATE MANIPULATION FUNCTIONS=========*)

(** [pipeline is_mv mv pc finish state] changes [state] depending on the
    legal second tile [finish], whether the change [is_move], and
    whether it is undergoing multicapture. Precondition: Either a move
    or capture is possible. [coord] is valid move or capture location of
    selected piece. *)
let pipeline (is_mv : bool) (mv : move) (state : t) : t =
  state |> add_mv mv |> mv_st false mv
  |> (if is_mv then Fun.id else cap_st mv)
  |> reset_st |> switch_tn
  |> (if is_mv then Fun.id else check_mc false mv)
  |> pro_pc mv
  |> if is_mv then Fun.id else check_vc

(** Similar to [reg_move] but uses memory from [state.redos] to perform
    the move and does not clear [state.redos].*)
let redo_move (state : t) : t =
  let r = Stack.pop state.redos in
  let is_mv = not (bmatcher r.cap_pc) in
  pipeline is_mv r state

(* TODO: Check for umpromotion. *)

(** [undo_move state] Relies heavily on preconditions. Precondition:
    [state] has a move to undo.*)
let undo_move (state : t) : t =
  let u = Stack.pop state.undos in
  Stack.push u state.redos;
  state
  |> (if u.cap_pc |> bmatcher then uncap_st u else Fun.id)
  |> mv_st true u |> reset_st
  |> (if u.mc_pres then check_mc true u else rem_mc)
  |> set_tn u.pc.player

(** [check_sf mv finish state]*)
let match_mv (r : move) (mv : move) : bool =
  r.start = mv.start && r.finish = mv.finish && r.pc = mv.pc

(* [reg_move is_mv finish state]*)
let reg_move (is_mv : bool) (finish : int * int) (state : t) : t =
  let mv = create_mv is_mv finish state in
  if Stack.length state.redos < 1 then pipeline is_mv mv state
  else
    let r = Stack.top state.redos in
    if match_mv r mv then redo_move state
    else begin
      Stack.clear state.redos;
      pipeline is_mv mv state
    end

(*=========TURN INTERFACE & IMMEDIATE HFs=========*)

(** [turn] represents the type of state returned. Continue represents
    that the current player stays the same and requires new input. Legal
    represents that the board and player has changed. Illegal is similar
    to Continue but the player has inputted an invalid set of moves.*)
type turn =
  | Continue of t
  | Legal of t
  | Illegal of t
  | NoUndo of t
  | NoRedo of t

(** [legal_action coord state] returns whether second click [coord] is
    located within the possible moves or captures of [state].*)
let legal_act (coord : int * int) (state : t) : bool =
  match_coord coord state.moves || match_coord coord (fst state.caps)

(** [legal_mc coord state] handles move legality when there is a forced
    multicapture for one player depending on [coord] and [state].*)
let legal_mc (coord : int * int) (state : t) : turn =
  if match_coord coord (fst state.caps) then
    Legal (reg_move false coord state)
  else Illegal state

let update (coord : int * int) (state : t) : turn =
  if state.mc_pres then legal_mc coord state
  else if valid_fst_clk coord state then
    Continue (store_fst_clk coord state)
  else if (not (unselected state)) && legal_act coord state then
    let legal_move = match_coord coord state.moves in
    Legal (reg_move legal_move coord state)
  else if valid_reclk coord state then
    Continue (store_fst_clk coord state)
  else if unselected state then Continue state
  else Illegal state

(** [check_do f stack state]*)
let check_do
    (f : t -> t)
    (undo : bool)
    (stack : move Stack.t)
    (state : t) : turn =
  if Stack.length stack < 1 then
    if undo then NoUndo state else NoRedo state
  else Legal (f state)

let urdo (undo : bool) (state : t) : turn =
  if undo then check_do undo_move undo state.undos state
  else check_do redo_move undo state.redos state

let match_turn cf lf ilf uf rf (tn : turn) =
  match tn with
  | Continue s -> cf s
  | Legal s -> lf s
  | Illegal s -> ilf s
  | NoUndo s -> uf s
  | NoRedo s -> rf s

let get_state (tn : turn) : t =
  match_turn Fun.id Fun.id Fun.id Fun.id Fun.id tn
