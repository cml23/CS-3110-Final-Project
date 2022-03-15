open Game
open Graphics
(** [main ()] prompts for the game to play, then starts it. *)
let main () = Game.Canvas.draw (Game.State.init_state Game.Board.init_board);
 let ev = wait_next_event [Key_pressed; Button_down;] in
 synchronize ();
 if ev.button then () else if ev.keypressed then ();;
 

(* Execute the game engine. *)
let () = main ()
