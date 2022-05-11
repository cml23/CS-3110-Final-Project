exception NoMove
(** If the AI is unable to make any moves, throw this exception. The
    game should never pass a state where the AI cannot make any moves. *)

(** [get_loc_mvs st pc] returns the location and moves (moves and
    captures) of a single piece. *)
let get_loc_mvs (st : State.t) (pc : Board.piece) :
    (int * int) * (int * int) list * (int * int) list =
  let bd = State.get_board st in
  let loc = State.get_xy_of_pc pc bd in
  let mvs = Board.poss_moves bd pc in
  let caps = fst (Board.poss_captures bd pc) in
  (loc, mvs, caps)

(** [get_plays st] returns a list of triples. For each triple, the first
    value is the coordinate of one of the ai's pieces, the second value
    is a list of possible moves for that piece, and the third value is a
    list of possible captures for that piece. *)
let get_plays (st : State.t) :
    ((int * int) * (int * int) list * (int * int) list) list =
  let pcs = Board.pieces_of_player (State.get_board st) 2 in
  List.map (get_loc_mvs st) pcs

(** [has_captures plays_of_pc] is true if the list [plays_of_pc] has a
    list of nonzero length for its captures list; false otherwise. *)
let has_captures
    (plays_of_pc : (int * int) * (int * int) list * (int * int) list) :
    bool =
  match plays_of_pc with
  | loc, mvs, caps -> List.length caps > 0

(** [has_moves plays_of_pc] is true if the list [plays_of_pc] has a list
    of nonzero length for its moves list; false otherwise. *)
let has_moves
    (plays_of_pc : (int * int) * (int * int) list * (int * int) list) :
    bool =
  match plays_of_pc with
  | loc, mvs, caps -> List.length mvs > 0

(** [find_captures plays] is the list of piece locations and
    corresponding captures lists for [plays], excluding andy pieces that
    cannot make captures. *)
let find_captures
    (plays : ((int * int) * (int * int) list * (int * int) list) list) :
    ((int * int) * (int * int) list) list =
  List.map (fun (a, b, c) -> (a, c)) (List.filter has_captures plays)

(** [find_moves plays] is the list of piece locations and corresponding
    moves lists for [plays], excluding any pieces that cannot make
    moves. *)
let find_moves
    (plays : ((int * int) * (int * int) list * (int * int) list) list) :
    ((int * int) * (int * int) list) list =
  List.map (fun (a, b, c) -> (a, b)) (List.filter has_moves plays)

(** [sel_play plays] is a play (an initial and final piece location. If
    the AI can make a capture, then a random capture is selected.
    Otherwise, a random move is selected. *)
let sel_play
    (plays : ((int * int) * (int * int) list * (int * int) list) list) :
    (int * int) * (int * int) =
  let poss_captures = find_captures plays in
  let poss_moves = find_moves plays in
  let play_list =
    if List.length poss_captures > 0 then poss_captures else poss_moves
  in
  let fsti = Random.int (List.length play_list) in
  let selp = List.nth play_list fsti in
  let sndl = selp |> snd in
  let sndi = Random.int (sndl |> List.length) in
  (fst selp, List.nth sndl sndi)

(** [no_mv bd] checks whether the ai can make a move or not. *)
let no_play (bd : Board.t) : bool = Board.num_pcs_of_pl bd 2 = 0

let make_mv (st : State.t) : State.turn =
  if no_play (State.get_board st) then raise NoMove
  else
    let play = st |> get_plays |> sel_play in
    st
    |> State.update (fst play)
    |> State.get_state
    |> State.update (snd play)
