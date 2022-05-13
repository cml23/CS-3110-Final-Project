exception NoMove
(** Compilation module that encompasses AI decision.*)

val make_mv : State.t -> State.turn
(** [make_mv state] takes a game state and attempts to make a turn. If
    there are possible captures, the move is chosen randomly from all
    possible captures. Otherwise the move will be randomly chosen from
    the set of all legal moves. If the AI has not pieces left, it simply
    returns. Should always return a [Legal] or [Continue] turn. *)
