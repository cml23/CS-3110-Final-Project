open Game

(* Howard Start *)

type t = {
  p1_sc : int;
  p2_sc : int;
  state : State.t;
  turn : State.turn;
  preset : int;
  use_ai : bool;
}

(* [default_game]*)
let file_format = ".json"

let default_game = "default_layout"

let data_dir_prefix = "data" ^ Filename.dir_sep

let game_name = ref (data_dir_prefix ^ "default_layout" ^ file_format)

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

let from_json_state json =
  let open Yojson.Basic.Util in
  State.init_state
    (json |> member "turn" |> to_int)
    (Board.from_json (json |> member "board"))

let from_json (use_ai1 : bool) (json : Yojson.Basic.t) : t =
  let open Yojson.Basic.Util in
  let st = from_json_state json in
  {
    p1_sc = json |> member "player 1 score" |> to_int;
    p2_sc = json |> member "player 2 score" |> to_int;
    preset = json |> member "preset" |> to_int;
    state = st;
    turn = Continue st;
    use_ai = use_ai1;
  }

let save_game (g : t) (name : string) : unit =
  Yojson.Basic.to_file
    (data_dir_prefix ^ name ^ file_format)
    (to_json g)

(* END functions added by Anirudh. *)

(*=========GAME MANIPULATION FUNCTIONS=========*)

let is_p1_win (game : t) : bool =
  if not (game.state |> State.game_over) then
    failwith "Called when game is not over"
  else game.state |> State.get_vc = 1

let change_st (chg : 'a -> State.t -> State.turn) (v : 'a) (game : t) :
    t =
  let nt = chg v game.state in
  { game with state = State.get_state nt; turn = nt }

let process_mv (coord : int * int) (game : t) : t =
  change_st State.update coord game

let urdo_mv (undo : bool) (game : t) : t =
  change_st State.urdo undo game

let change_sc (p1_win : bool) (game : t) : t =
  let new_game =
    {
      game with
      p1_sc = (game.p1_sc + if p1_win then 1 else 0);
      p2_sc = (game.p2_sc + if not p1_win then 1 else 0);
    }
  in
  print_endline (string_of_int new_game.p1_sc);
  print_endline (string_of_int new_game.p2_sc);
  new_game

let restart_gm (game : t) : t =
  let new_state =
    !game_name |> Yojson.Basic.from_file |> from_json_state
  in
  { game with state = new_state; turn = Legal new_state }

let change_preset (game : t) : t =
  { game with preset = Game.Canvas.swap_preset game.preset }

let check_win (game : t) : t =
  if game.state |> State.game_over then (
    let new_sc_game =
      game |> change_sc (game |> is_p1_win) |> restart_gm
    in
    Canvas.draw_new_game game.preset game.p1_sc game.p2_sc game.state;
    new_sc_game)
  else game

(*=========DRAW FUNCTIONS========*)
let draw_st (game : t) : _ = game.state |> Game.Canvas.draw game.preset

let highlight (e : Graphics.status) (game : t) : _ =
  game.state |> State.get_board |> Canvas.highlight e

(*=========GAME LOOP=========*)
(* [glref event game] stores [game_loop] as a *)
let gl_ref = ref (fun a b -> ())

(* [event_handler glref game]*)
let rec event_handler glref (game : t) : _ =
  let event = Graphics.wait_next_event [ Button_down; Key_pressed ] in
  let gle = !glref event in
  if event.button then
    match Canvas.mouse_input event (game.state |> State.get_board) with
    | Some coord -> game |> process_mv coord |> gle
    | None -> game |> gle
  else if event.key = 'z' then game |> urdo_mv true |> gle
  else if event.key = 'x' then game |> urdo_mv false |> gle
  else if event.key = 'r' then game |> restart_gm |> gle
  else if event.key = 'c' then game |> change_preset |> gle
  else if event.key = 's' then (
    save_game game "saved_game";
    exit 0)
  else if event.key = 'q' then exit 0
  else game |> gle;
  ()

(* [game_loop e game]*)
let rec game_loop (e : Graphics.status) (game : t) : _ =
  (* if State.game_over game.state then let game = (change_sc game); *)
  draw_st game;
  let game = game |> check_win in
  if game.use_ai && State.get_player game.state = 2 then
    try
      let new_turn = Ai.make_mv game.state in
      Unix.sleep 1;
      let gm =
        { game with turn = new_turn; state = State.get_state new_turn }
      in
      game_loop e gm
    with
    | Game.Ai.NoMove ->
        game |> change_sc true |> restart_gm |> game_loop e
    | current_game -> ()
  else if e.button then highlight e game
  else ();
  event_handler gl_ref game

(* [start_game game]*)
let start_game game =
  Canvas.draw_new_game game.preset game.p1_sc game.p2_sc game.state;
  gl_ref := game_loop;
  event_handler gl_ref game;
  ()

(* [gf_ref] stores [game_loop] as a *)
(* let gf_ref = ref (fun a -> ()) *)

let rec get_file loader =
  match read_line () with
  | exception End_of_file -> ()
  | "default" -> loader (data_dir_prefix ^ default_game ^ file_format)
  | file_name -> loader (data_dir_prefix ^ file_name ^ file_format)

(* [load_game f]*)
let rec load_game (use_ai : bool) (f : string) =
  try
    game_name := f;
    f |> Yojson.Basic.from_file |> from_json use_ai |> start_game
  with _ ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Invalid file name, please try again.\n";
    print_endline
      "Please enter the name of the game file you want to load.\n";
    Stdlib.print_string "> ";
    get_file (load_game use_ai)

(**[print_name_list lst] prints the preset list of player names given in
   [lst] to the terminal.*)
let rec print_name_list lst =
  let player_names = List.filter (fun name -> name <> "AI") lst in
  List.iter (fun name -> print_endline ("-" ^ name)) player_names;
  print_endline ""

(**[get_name loader] assigns the name input by the user to a player if
   it is a valid name choice (based on the preset list given in
   [Game.Canvas])*)
let rec get_name loader =
  match read_line () with
  | exception End_of_file ->
      "That is not a valid name. Please try again. \n"
  | s -> begin
      match
        List.filter (fun y -> y = s && y <> "AI") Canvas.player_names
      with
      | [] ->
          print_endline "That is not a valid name. Please try again.\n";
          print_string "> ";
          get_name ()
      | h :: t -> h
    end

(**[get_player_name player name] assigns a [name] to [player] based on
   user input.*)
let get_player_name player name =
  print_endline
    ("Please choose Player " ^ string_of_int player ^ " name.");
  print_string "> ";
  name := get_name ();
  print_endline ""

let rec get_yes_no (loader : unit) : bool =
  match read_line () with
  | "1" -> true
  | "2" -> false
  | _ ->
      print_endline "Please choose 1 or 2. \n";
      get_yes_no ()

let get_ai () : bool =
  print_endline "Do you have 1 or 2 players?";
  print_string "> ";
  let use_ai = get_yes_no () in
  print_endline "";
  use_ai

(** [main] starts a game based on .*)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nOCaml Checkers Initialized\n";
  print_endline "\nAvailable player names: ";
  print_name_list Canvas.player_names;
  let use_ai = get_ai () in
  get_player_name 1 Canvas.p1_name;
  if use_ai then Canvas.p2_name := "AI"
  else get_player_name 2 Canvas.p2_name;
  print_endline
    "Please enter the name of the save file you want to load.\n";
  print_endline
    "Or enter \"default\" to start a standard checkers game.";
  print_string "> ";
  get_file (load_game use_ai)

(* Execute the game engine. *)
let () = main ()

(* Howard End *)
