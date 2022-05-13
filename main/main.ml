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
(** Type for the game that keeps track of player score, the state of the
    board, the type of turn that has just occurred, the visual preset
    being used, and whether ai is in use or not. *)

let file_format = ".json"
(* [fileformat] is the file type for saved files. *)

let default_game = "default_layout"
(* [default_game] is the filename for the default board. *)

let data_dir_prefix = "data" ^ Filename.dir_sep
(* [data_dir_prefix] is the filelcations for saved files. *)

let game_name = ref (data_dir_prefix ^ "default_layout" ^ file_format)
(* [game_name] is the current file being used to play the game. *)

(*=========LOADERS=========*)

(* Functions added by Anirudh *)

(* The json representation of the game stores the current board, the
   current player turn, and the multi-game score tally. *)

(** [from_json_state json] only returns the [game.state] constructed
    from [json] for the purposes of restarting a game. *)
let from_json_state json =
  let open Yojson.Basic.Util in
  State.init_state
    (json |> member "turn" |> to_int)
    (Board.from_json (json |> member "board"))

(** [from_json useai1 json] creates a new game state based on [json] and
    whether [useai1] is true or not.*)
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

(** [to_json] converts a game into a save file with board, turn, player
    score, and preset information svaed. *)
let to_json (g : t) : Yojson.Basic.t =
  `Assoc
    [
      ("board", Board.to_json (State.get_board g.state));
      ("turn", `Int (State.get_player g.state));
      ("player 1 score", `Int g.p1_sc);
      ("player 2 score", `Int g.p2_sc);
      ("preset", `Int g.preset);
    ]

(** [save_game g name] converts a game into a save file. *)
let save_game (g : t) (name : string) : unit =
  Yojson.Basic.to_file
    (data_dir_prefix ^ name ^ file_format)
    (to_json g)

(* END functions added by Anirudh. *)

(*=========GAME MANIPULATION FUNCTIONS=========*)

(** [is_p1_win game] returns whether player 1 has won or not.
    Precondition: the game is over. *)
let is_p1_win (game : t) : bool =
  if not (game.state |> State.game_over) then
    failwith "Called when game is not over"
  else game.state |> State.get_vc = 1

(** [change_st chg v game] takes a function [chg] that modifies
    [game.state] and applies it to [game] with value [v]. *)
let change_st (chg : 'a -> State.t -> State.turn) (v : 'a) (game : t) :
    t =
  let nt = chg v game.state in
  { game with state = State.get_state nt; turn = nt }

(** [process_mv] is a wrapper function for the [update] function in
    [State] to pass a selected square to state. *)
let process_mv (coord : int * int) (game : t) : t =
  change_st State.update coord game

(** [undo_mv undo game] is a wrapper function for the [urdo] function in
    [State] to return the game state to the previous move. *)
let urdo_mv (undo : bool) (game : t) : t =
  change_st State.urdo undo game

(** [change_sc game] updates the scores by one for player 1 if [p1_win]
    is [true] and for player 2 otherwise. Precondition: the game is
    over. *)
let change_sc (p1_win : bool) (game : t) : t =
  {
    game with
    p1_sc = (game.p1_sc + if p1_win then 1 else 0);
    p2_sc = (game.p2_sc + if not p1_win then 1 else 0);
  }

(** [restart_gm game] reloads the game based on the file name selected. *)
let restart_gm (game : t) : t =
  let new_state =
    !game_name |> Yojson.Basic.from_file |> from_json_state
  in
  { game with state = new_state; turn = Legal new_state }

(** [change_preset game] changes the preset number to next one available
    for screen recoloring. *)
let change_preset (game : t) : t =
  { game with preset = Game.Canvas.swap_preset game.preset }

(*=========DRAW FUNCTIONS========*)

(** [draw_st game] calls the canvas [draw] function with preset
    information. *)
let draw_st (game : t) : _ = game.state |> Game.Canvas.draw game.preset

(** [draw_sc game] calls the canvas [draw_new_game] function with preset
    and score information. *)
let draw_sc (game : t) : _ =
  Canvas.draw_new_game game.preset game.p1_sc game.p2_sc game.state

(** [highlight e game] highlights the specific square that has been
    selected by calling the canvas [highlight] function. *)
let highlight (e : Graphics.status) (game : t) : _ =
  game.state |> State.get_board |> Canvas.highlight e

let draw_sc_pipeline (p1w : bool) (game : t) : t =
  let nsc_gm = game |> change_sc p1w |> restart_gm in
  draw_sc nsc_gm;
  nsc_gm

(*=========GAME LOOP=========*)
(* [glref event game] stores [game_loop] as a reference to allow access
   throughout the program. *)
let gl_ref = ref (fun a b -> ())

(** [forfeit game] restarts the board so the player who did not forfeit
    gains one point in score. *)
let forfeit (game : t) : t =
  let p1w =
    if game.state |> State.get_player = 1 then false else true
  in
  game |> draw_sc_pipeline p1w

(** [check_win glref e game] checks for two player games whether one
    player can move or not and updates the score accordingly. *)
let check_win glref (e : Graphics.status) (game : t) : _ =
  if game.state |> State.game_over then (
    Unix.sleepf 1.0;
    game |> draw_sc_pipeline (game |> is_p1_win) |> !gl_ref e)

(** [check_ai glref e game] makes a move with the AI if the player has
    opted for PvE. Also checks for the win condition when the AI cannot
    make any new moves.*)
let check_ai glref (e : Graphics.status) (game : t) : _ =
  if game.use_ai && State.get_player game.state = 2 then
    try
      let new_turn = Ai.make_mv game.state in
      Unix.sleep 1;
      let gm =
        { game with turn = new_turn; state = State.get_state new_turn }
      in
      !glref e gm
    with
    | Game.Ai.NoMove ->
        Unix.sleepf 1.0;
        game |> draw_sc_pipeline true |> !glref e
    | current_game -> ()

(* [event_handler glref game] performs operations on the game state
   depending on the input from the user. "z" undos a move, "x" redos a
   move, "r" restarts the game, keeping the score, "c" changes the color
   of the game, "s" saves the game, "q" quits the game.*)
let rec event_handler glref (game : t) : _ =
  let event = Graphics.wait_next_event [ Button_down; Key_pressed ] in
  let gle = !glref event in
  if event.button then
    match Canvas.mouse_input event (game.state |> State.get_board) with
    | Some coord -> game |> process_mv coord |> gle
    | None -> game |> gle
  else if event.key = 'z' then game |> urdo_mv true |> gle
  else if event.key = 'x' then game |> urdo_mv false |> gle
  else if event.key = 'f' then game |> forfeit |> gle
  else if event.key = 'r' then game |> restart_gm |> gle
  else if event.key = 'c' then game |> change_preset |> gle
  else if event.key = 's' then (
    save_game game "saved_game";
    exit 0)
  else if event.key = 'q' then exit 0
  else game |> gle;
  ()

(* [game_loop e game] is the main body of the game, which draws the
   board, checks the win conditions, makes ai moves, highlights the
   selected square. If nothing special happens, called
   [event_handler]. *)
let rec game_loop (e : Graphics.status) (game : t) : _ =
  (* if State.game_over game.state then let game = (change_sc game); *)
  draw_st game;
  check_win gl_ref e game;
  check_ai gl_ref e game;
  if e.button then highlight e game else ();
  event_handler gl_ref game

(* [start_game game] takes in a [game], sets the game loop reference,
   and begins the main game loop. *)
let start_game game =
  Canvas.draw_new_game game.preset game.p1_sc game.p2_sc game.state;
  gl_ref := game_loop;
  event_handler gl_ref game;
  ()

(* [get_file loader] takes a filename and appends to necessary
   information to for parsing. *)
let rec get_file loader =
  match read_line () with
  | exception End_of_file -> ()
  | "default" -> loader (data_dir_prefix ^ default_game ^ file_format)
  | file_name -> loader (data_dir_prefix ^ file_name ^ file_format)

(* [load_game use_ai f] constructs a game of type t from filename [f]
   and sets whether to use the ai based on [use_ai] and starts the game
   with said state. *)
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

(** [get_yes_no] is a helper function for [get_ai]. Returns [true] if
    the player wants an ai by typing ["1"]. *)
let rec get_yes_no () : bool =
  match read_line () with
  | "1" -> true
  | "2" -> false
  | _ ->
      print_endline "Please choose 1 or 2. \n";
      get_yes_no ()

(** [get_ai] asks the player in the terminal whether they want to have
    an ai or not. Returns [true] if the player wants an ai. *)
let get_ai () : bool =
  print_endline "Do you have 1 or 2 players?";
  print_string "> ";
  let use_ai = get_yes_no () in
  print_endline "";
  use_ai

(** [main] starts a game, and asks the player a series of questions to
    set up the game.*)
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
