exception NoMove
(** If the AI is unable to make any moves, throw this exception. The
    game should never pass a state where the AI cannot make any moves. *)

(** [get_loc_mvs st pc] returns the location and moves of a single
    piece. *)
let get_loc_mvs (st : State.t) (pc : Board.piece) :
    (int * int) * (int * int) list =
  let bd = State.get_board st in
  let loc = State.get_xy_of_pc pc bd in
  let mvs = Board.poss_moves bd pc in
  let caps = fst (Board.poss_captures bd pc) in
  (loc, mvs @ caps)

(** [get_mvs st] returns a list of coordinates of the the ai's pieces
    and each piece's legal moves. *)
let get_mvs (st : State.t) : ((int * int) * (int * int) list) list =
  let pcs = Board.pieces_of_player (State.get_board st) 2 in
  List.map (get_loc_mvs st) pcs

let sel_mv (mvs : ((int * int) * (int * int) list) list) :
    (int * int) * (int * int) =
  let fsti = Random.int (List.length mvs) in
  let selp = List.nth mvs fsti in
  let sndl = selp |> snd in
  let sndi = Random.int (sndl |> List.length) in
  (fst selp, List.nth sndl sndi)

(** [no_mv bd] checks whether the ai an make a move or not. *)
let no_mv (bd : Board.t) : bool = Board.num_pcs_of_pl bd 2 = 0

let make_mv (st : State.t) : State.turn =
  if no_mv (State.get_board st) then raise NoMove
  else
    let mv = st |> get_mvs |> sel_mv in
    st
    |> State.update (fst mv)
    |> State.get_state
    |> State.update (snd mv)
