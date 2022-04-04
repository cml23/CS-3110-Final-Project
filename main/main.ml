open Game
open Graphics

(**[play_game ev] handles input using the Graphics status ev and
   controls the logic of the game.*)

type t = { state : State.t }

let draw_state (game : t) : _ = Game.Canvas.draw game.state

let process_move (coord : int * int) (game : t) : t =
  {
    state =
      (match State.update game.state coord with
      | Continue s -> s
      | Legal s -> s
      | Illegal s -> s);
  }

let urdo_move (undo : bool) (game : t) : t =
  {
    state =
      (match State.urdo undo game.state with
      | Continue s -> s
      | Legal s -> s
      | Illegal s -> s);
  }

let rec game_loop (game : t) (e : Graphics.status) : _ =
  draw_state game;
  if e.button then Canvas.highlight e (State.get_board game.state)
  else ();
  let event = Graphics.wait_next_event [ Button_down; Key_pressed ] in
  if event.button then
    match Canvas.mouse_input event Game.Board.init_board with
    | Some coord -> game_loop (process_move coord game) event
    | None -> game_loop game event
  else if event.key = 'z' then game_loop (urdo_move true game) event
  else if event.key = 'x' then game_loop (urdo_move false game) event
  else if event.keypressed then exit 0
  else game_loop game event

let main () =
  let game = { state = State.init_state Board.init_board } in
  draw_state game;
  let event = Graphics.wait_next_event [ Button_down; Key_pressed ] in
  if event.button then
    match Canvas.mouse_input event Game.Board.init_board with
    | Some coord -> game_loop (process_move coord game) event
    | None -> game_loop game event
  else if event.key = 'z' then game_loop (urdo_move true game) event
  else if event.key = 'x' then game_loop (urdo_move false game) event
  else if event.keypressed then exit 0
  else game_loop game event

(* Execute the game engine. *)
let () = main ()
