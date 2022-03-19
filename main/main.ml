open Game
open Graphics

(**[play_game ev] handles input using the Graphics status ev and
   controls the logic of the game.*)
let play_game ev =
  if ev.button then Canvas.highlight ev Game.Board.init_board
  else if ev.keypressed then exit 0
  else ()

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  Game.Canvas.draw (Game.State.init_state Game.Board.init_board);
  Graphics.loop_at_exit [ Key_pressed; Button_down ] play_game
(**let ev = wait_next_event [Key_pressed; Button_down;] in if ev.button
   then Canvas.mouse_input ev Game.Board.init_board else (); synchronize
   (); main()*)

(* Execute the game engine. *)
let () = main ()
