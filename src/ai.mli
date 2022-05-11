(** Compilation module that encompasses AI decision.*)

val make_mv : State.t -> State.turn
(** [make_mv state] takes a game state and attempts to make a turn. The
    move it makes will be randomly chosen from the set of all legal
    moves. If the AI has not pieces left, it simply returns. Should
    always return a [Legal] turn. *)
