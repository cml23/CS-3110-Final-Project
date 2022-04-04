open Game
open Graphics

type t = {
  p1_sc : int;
  p2_sc : int;
  state : State.t;
  turn : State.turn;
  flip : bool;
}

let init_st = State.init_state Board.init_board

let init_game =
  let s = init_st in
  { p1_sc = 0; p2_sc = 0; state = s; turn = Legal s; flip = false }

(*=========GAME MANIPULATION FUNCTIONS=========*)

let match_turn contf legf illegf (tn : State.turn) =
  match tn with
  | Continue s -> contf s
  | Legal s -> legf s
  | Illegal s -> illegf s

let get_state (tn : State.turn) : State.t =
  match_turn Fun.id Fun.id Fun.id tn

let toggle_fl (game : t) : t = { game with flip = not game.flip }

let change_st (chg : 'a -> State.t -> State.turn) (v : 'a) (game : t) :
    t =
  let nt = chg v game.state in
  { game with state = get_state nt; turn = nt }

let process_mv (coord : int * int) (game : t) : t =
  change_st State.update coord game

let urdo_mv (undo : bool) (game : t) : t =
  change_st State.urdo undo game

let change_sc (game : t) : t =
  {
    game with
    p1_sc = game.p1_sc + State.get_pts 1 game.state;
    p2_sc = game.p2_sc + State.get_pts 2 game.state;
  }

let restart_gm (game : t) : t = { game with turn = Legal init_st }

(*=========DRAW FUNCTIONS========*)
let draw_st (game : t) : _ = game.state |> Game.Canvas.draw
let draw_cont (st : State.t) : _ = ()
let draw_leg (st : State.t) : _ = ()
let draw_illeg (st : State.t) : _ = ()

let draw_turn (game : t) : _ =
  match_turn draw_cont draw_leg draw_illeg game.turn

let highlight (e : Graphics.status) (game : t) : _ =
  game.state |> State.get_board |> Canvas.highlight e

(*=========GAME LOOP=========*)
let glref = ref (fun a b -> ())

let rec event_handler glref (game : t) : _ =
  let event = Graphics.wait_next_event [ Button_down; Key_pressed ] in
  let gle = !glref event in
  if event.button then
    match Canvas.mouse_input event Game.Board.init_board with
    | Some coord -> game |> process_mv coord |> gle
    | None -> game |> gle
  else if event.key = 'z' then game |> urdo_mv true |> gle
  else if event.key = 'x' then game |> urdo_mv false |> gle
  else if event.key = 'r' then game |> restart_gm |> gle
  else if event.key = 'f' then game |> toggle_fl |> gle
  else if event.key = 'q' then exit 0
  else game |> gle;
  ()

let rec game_loop (e : Graphics.status) (game : t) : _ =
  (* if State.game_over game.state then let game = (change_sc game); *)
  draw_st game;
  draw_turn game;
  if e.button then highlight e game else ();
  event_handler glref game;
  ()

(** [main] starts the game an initiliazes a reference to the game loop
    to reduce redundant code.*)
let main () =
  let game = init_game in
  draw_st game;
  glref := game_loop;
  event_handler glref game;
  ()

(* Execute the game engine. *)
let () = main ()
