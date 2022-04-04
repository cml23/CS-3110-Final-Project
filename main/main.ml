open Game
open Graphics

(**[play_game ev] handles input using the Graphics status ev and
   controls the logic of the game.*)

type t = {
  p1_sc : int;
  p2_sc : int;
  state : State.t;
}

let draw_state (game : t) : _ = Game.Canvas.draw game.state
let init_st = State.init_state Board.init_board

let process_move (coord : int * int) (game : t) : t =
  {
    game with
    state =
      (match State.update game.state coord with
      | Continue s -> s
      | Legal s -> s
      | Illegal s -> s);
  }

let urdo_move (undo : bool) (game : t) : t =
  {
    game with
    state =
      (match State.urdo undo game.state with
      | Continue s -> s
      | Legal s -> s
      | Illegal s -> s);
  }

let change_score (game : t) : t =
  {
    p1_sc = game.p1_sc + State.get_pts 1 game.state;
    p2_sc = game.p2_sc + State.get_pts 2 game.state;
    state = init_st;
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
  else if event.key = 'r' && State.game_over game.state then
    game_loop (change_score game) e
  else if event.keypressed then exit 0
  else game_loop game event

let main () =
  let game = { p1_sc = 0; p2_sc = 0; state = init_st } in
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
