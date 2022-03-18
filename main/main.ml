open Game
open Graphics
(** [main ()] prompts for the game to play, then starts it. *)
let rec main () = 
 Game.Canvas.draw (Game.State.init_state Game.Board.init_board);
 Graphics.loop_at_exit [Key_pressed; Button_down] (fun x -> begin
  if x.button then Canvas.mouse_input x Game.Board.init_board 
  else if x.keypressed then exit 0 else () end )
 (**let ev = wait_next_event [Key_pressed; Button_down;] in
 if ev.button then Canvas.mouse_input ev Game.Board.init_board else ();
 synchronize ();
 main()*)

(* Execute the game engine. *)
let () = main ()
