open Game

(* Howard Start *)

type t = {
  p1_sc : int;
  p2_sc : int;
  state : State.t;
  turn : State.turn;
  preset : int;
}

let init_st = State.def_state

let init_game =
  Game.Canvas.init;
  let s = init_st in
  { p1_sc = 0; p2_sc = 0; state = s; turn = Legal s; preset = 0 }

(*=========GAME MANIPULATION FUNCTIONS=========*)

let change_st (chg : 'a -> State.t -> State.turn) (v : 'a) (game : t) :
    t =
  let nt = chg v game.state in
  { game with state = State.get_state nt; turn = nt }

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

let change_preset (game : t) : t =
  { game with preset = Game.Canvas.swap_preset game.preset }

(*=========DRAW FUNCTIONS========*)
let draw_st (game : t) : _ = game.state |> Game.Canvas.draw game.preset
let dc (st : State.t) : _ = ()
let dl (st : State.t) : _ = ()
let di (st : State.t) : _ = ()
let du (st : State.t) : _ = ()
let dr (st : State.t) : _ = ()
let draw_turn (game : t) : _ = State.match_turn dc dl di du dr game.turn

let highlight (e : Graphics.status) (game : t) : _ =
  game.state |> State.get_board |> Canvas.highlight e

(*=========LOADERS=========*)

(* Functions added by Anirudh *)

(* The json representation of the game stores the current board, the
   current player turn, and the multi-game score tally. *)

let to_json (g : t) : Yojson.Basic.t =
  `Assoc
    [
      ("board", Board.to_json (State.get_board g.state));
      ("turn", `Int (State.get_player g.state));
      ("player 1 score", `Int g.p1_sc);
      ("player 2 score", `Int g.p2_sc);
      ("preset", `Int g.preset);
    ]

let from_json json : t =
  let open Yojson.Basic.Util in
  let st =
    State.init_state
      (json |> member "turn" |> to_int)
      (Board.from_json (json |> member "board"))
  in
  {
    p1_sc = json |> member "player 1 score" |> to_int;
    p2_sc = json |> member "player 2 score" |> to_int;
    preset = json |> member "preset" |> to_int;
    state = st;
    turn = Continue st;
  }

(* END functions added by Anirudh. *)

(*=========GAME LOOP=========*)
(* [glref event game] stores [game_loop] as a *)
let gl_ref = ref (fun a b -> ())

(* [event_handler glref game]*)
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
  else if event.key = 'c' then game |> change_preset |> gle
  else if event.key = 's' then game |> to_json |> ignore
  else if event.key = 'q' then exit 0
  else game |> gle;
  ()

(* [game_loop e game]*)
let rec game_loop (e : Graphics.status) (game : t) : _ =
  (* if State.game_over game.state then let game = (change_sc game); *)
  draw_st game;
  draw_turn game;
  if e.button then highlight e game else ();
  event_handler gl_ref game;
  ()

(* [start_game game]*)
let start_game game =
  draw_st game;
  gl_ref := game_loop;
  event_handler gl_ref game;
  ()

(* [default_game]*)
let default_game = init_game |> start_game
let data_dir_prefix = "data" ^ Filename.dir_sep

(* [gf_ref] stores [game_loop] as a *)
(* let gf_ref = ref (fun a -> ()) *)

let rec get_file loader =
  match read_line () with
  | exception End_of_file -> ()
  | "default" -> default_game
  | file_name -> loader (data_dir_prefix ^ file_name ^ ".json")

(* [load_game f]*)
let rec load_game f =
  try f |> Yojson.Basic.from_file |> from_json |> start_game
  with _ -> (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Invalid file name, please try again.\n";
    print_endline
      "Please enter the name of the game file you want to load.\n";
    Stdlib.print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "default" -> default_game
    | file_name -> load_game (data_dir_prefix ^ file_name ^ ".json"))

(** [main] starts a game based on .*)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nOCaml Checkers Initialized\n";
  print_endline
    "Please enter the name of the save file you want to load.\n";
  print_endline
    "Or enter \"default\" to start a standard checkers game.";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "default" -> default_game
  | file_name -> load_game (data_dir_prefix ^ file_name ^ ".json")

(* Execute the game engine. *)
let () = main ()

(* Howard End *)
