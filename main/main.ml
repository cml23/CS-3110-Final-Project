open Game
(** [main ()] prompts for the game to play, then starts it. *)
let main () = Game.Canvas.draw (Game.State.init_state Game.Board.init_board)

(* Execute the game engine. *)
let () = main ()
