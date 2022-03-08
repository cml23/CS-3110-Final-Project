type t = {
  board : Board.t;
  win_state : bool;
  victor : string;
}

let init_state (board : Board.t) : t =
  { board; win_state = false; victor = "" }

let get_board (state : t) : Board.t =
  raise (Failure "Unimplemented: Board.pieces_of_player")

let game_over (state : t) : bool = state.win_state
let get_victor (state : t) : string = state.victor

type move =
  | Legal of t
  | Illegal

let go : string -> Adventure.t -> t -> move =
  raise (Failure "Unimplemented: Board.pieces_of_player")

let main () = raise (Failure "Unimplemented: Board.pieces_of_player")
(* Canvas.draw (init_state Board.init_board) *)

let () = main ()