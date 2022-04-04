open OUnit2
open Game.Command
open Game.State
open Game.Board
open Game.Canvas

(* TODO: add unit tests for modules below. You are free to reorganize
   the definitions below. Just keep it clear which tests are for which
   modules. *)

(* Add helper functions for testing Board here.*)

let print_int_pair (p : int * int) =
  "(" ^ string_of_int (fst p) ^ ", " ^ string_of_int (snd p) ^ ")"

let print_int_pair_opt (p : (int * int) option) =
  match p with
  | None -> "None"
  | Some (x, y) -> print_int_pair (x, y)

let rec print_int_pair_list (lst : (int * int) list) =
  match lst with
  | [] -> ""
  | h :: t -> print_int_pair h ^ ", " ^ print_int_pair_list t

let print_pc pc =
  "{player=" ^ string_of_int pc.player ^ "; id=" ^ string_of_int pc.id
  ^ "; is_royal="
  ^ string_of_bool pc.is_royal
  ^ "}"

let rec print_pc_lst lst =
  match lst with
  | [] -> ""
  | h :: t -> print_pc h ^ ", " ^ print_pc_lst t

let print_pc_opt pc_opt =
  match pc_opt with
  | None -> "None"
  | Some pc -> print_pc pc

let print_capture_lst lst =
  "Final locations: "
  ^ print_int_pair_list (fst lst)
  ^ "\n" ^ "Captured pieces: "
  ^ print_pc_lst (snd lst)

let def_bd = init_board
let def_bd_11del = del_pc def_bd 1 1

let dim_x_test (name : string) (b : Game.Board.t) (exp_out : int) : test
    =
  name >:: fun _ ->
  assert_equal exp_out (dim_x b) ~printer:string_of_int

let dim_y_test (name : string) (b : Game.Board.t) (exp_out : int) : test
    =
  name >:: fun _ ->
  assert_equal exp_out (dim_y b) ~printer:string_of_int

let piece_of_xy_test
    (name : string)
    (b : Game.Board.t)
    (x : int)
    (y : int)
    (exp_out : piece option) : test =
  name >:: fun _ ->
  assert_equal exp_out (piece_of_xy b x y) ~printer:print_pc_opt

let num_pcs_of_pl_test
    (name : string)
    (b : Game.Board.t)
    (pl : int)
    (exp_out : int) : test =
  name >:: fun _ ->
  assert_equal exp_out (num_pcs_of_pl b pl) ~printer:string_of_int

let xy_of_pc_test
    (name : string)
    (b : Game.Board.t)
    (pc : piece)
    (exp_out : (int * int) option) : test =
  name >:: fun _ ->
  assert_equal exp_out (xy_of_pc b pc) ~printer:print_int_pair_opt

let neigbor_test
    (name : string)
    adj_fun
    (b : Game.Board.t)
    (x : int)
    (y : int)
    (exp_out : (int * int) option) : test =
  name >:: fun _ ->
  assert_equal exp_out (adj_fun b x y) ~printer:print_int_pair_opt

let pc_exists_test
    (name : string)
    (b : Game.Board.t)
    (x : int)
    (y : int)
    (exp_out : bool) : test =
  name >:: fun _ ->
  assert_equal exp_out (pc_exists b x y) ~printer:string_of_bool

let poss_moves_test
    (name : string)
    (b : Game.Board.t)
    (pc : piece)
    (exp_out : (int * int) list) =
  name >:: fun _ ->
  assert_equal exp_out (poss_moves b pc) ~printer:print_int_pair_list

let poss_captures_test
    (name : string)
    (b : Game.Board.t)
    (pc : piece)
    (exp_out : (int * int) list * piece list) =
  name >:: fun _ ->
  assert_equal exp_out (poss_captures b pc) ~printer:print_capture_lst

let is_promotable_test
    (name : string)
    (b : Game.Board.t)
    (pc : piece)
    (exp_out : bool) =
  name >:: fun _ ->
  assert_equal exp_out (is_promotable b pc) ~printer:string_of_bool

let board_tests =
  [
    dim_x_test "default board x-dim" def_bd 8;
    dim_y_test "default board y-dim" def_bd 8;
    piece_of_xy_test "default board at 1,1" def_bd 1 1
      (Some { player = 1; id = 1; is_royal = false });
    piece_of_xy_test "default board at 6,2" def_bd 6 2
      (Some { player = 1; id = 7; is_royal = false });
    piece_of_xy_test "default board at 8,8" def_bd 8 8
      (Some { player = 2; id = 24; is_royal = false });
    piece_of_xy_test "default board at 6,4" def_bd 6 4 None;
    num_pcs_of_pl_test "default board, #pcs for pl1" def_bd 1 12;
    num_pcs_of_pl_test "default board, #pcs for pl2" def_bd 2 12;
    xy_of_pc_test "default board; piece at 1,1" def_bd
      { player = 1; id = 1; is_royal = false }
      (Some (1, 1));
    xy_of_pc_test "default board; piece at 8,8" def_bd
      { player = 2; id = 24; is_royal = false }
      (Some (8, 8));
    xy_of_pc_test "default board; nonexistent pc" def_bd
      { player = 1; id = 13; is_royal = false }
      None;
    neigbor_test "default board; up r of 1,1" up_r def_bd 1 1
      (Some (2, 2));
    neigbor_test "default board; up r of 8,8" up_r def_bd 8 8 None;
    neigbor_test "default board; up r of 8,1" up_r def_bd 8 1 None;
    neigbor_test "default board; up r of 1,8" up_r def_bd 1 8 None;
    neigbor_test "default board; up l of 8,1" up_l def_bd 8 1
      (Some (7, 2));
    neigbor_test "default board; up l of 1,8" up_l def_bd 1 8 None;
    neigbor_test "default board; up l of 8,8" up_l def_bd 8 8 None;
    neigbor_test "default board; up l of 1,1" up_l def_bd 1 1 None;
    neigbor_test "default board; down r of 1,8" down_r def_bd 1 8
      (Some (2, 7));
    neigbor_test "default board; down r of 8,8" down_r def_bd 8 8 None;
    neigbor_test "default board; down r of 8,1" down_r def_bd 8 1 None;
    neigbor_test "default board; down r of 1,1" down_r def_bd 1 1 None;
    neigbor_test "default board; down l of 8,8" down_l def_bd 8 8
      (Some (7, 7));
    neigbor_test "default board; down l of 1,8" down_l def_bd 1 8 None;
    neigbor_test "default board; down l of 8,1" down_l def_bd 8 1 None;
    neigbor_test "default board; down l of 1,1" down_l def_bd 1 1 None;
    pc_exists_test "default board; pc at 1,1" def_bd 1 1 true;
    pc_exists_test "default board; pc at 1,1" def_bd 8 8 true;
    pc_exists_test "default board; del pc at 1,1" def_bd_11del 1 1 false;
    poss_moves_test "def bd; poss moves of pc at 1,1" def_bd
      { player = 1; id = 1; is_royal = false }
      [];
    poss_moves_test "def bd; poss moves of pc at 1,3" def_bd
      { player = 1; id = 9; is_royal = false }
      [ (2, 4) ];
    poss_moves_test "def bd; poss moves of pc at 7,3" def_bd
      { player = 1; id = 12; is_royal = false }
      [ (8, 4); (6, 4) ];
    poss_moves_test "def bd; poss moves of pc at 2,8" def_bd
      { player = 2; id = 21; is_royal = false }
      [];
    poss_moves_test "def bd; poss moves of pc at 8,6" def_bd
      { player = 2; id = 16; is_royal = false }
      [ (7, 5) ];
    poss_moves_test "def bd; poss moves of pc at 2,6" def_bd
      { player = 2; id = 13; is_royal = false }
      [ (3, 5); (1, 5) ];
    poss_captures_test "def bd; poss captures of pc at 1,1" def_bd
      { player = 1; id = 1; is_royal = false }
      ([], []);
    poss_captures_test "def bd; poss captures of pc at 2,6" def_bd
      { player = 2; id = 13; is_royal = false }
      ([], []);
    is_promotable_test "def bd; no promotable pc at 1,1" def_bd
      { player = 1; id = 1; is_royal = false }
      false;
    is_promotable_test "def bd; no promotable pc at 8,6" def_bd
      { player = 2; id = 16; is_royal = false }
      false;
  ]

(* Add helper functions for testing State here.*)
let rec print_selected (selected : (int * int) list) =
  match selected with
  | [] -> ""
  | (x, y) :: t ->
      "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
      ^ print_selected t

let print_state (state : Game.State.t) =
  "{game_over="
  ^ (state |> game_over |> string_of_bool)
  ^ ";\n victor=" ^ (state |> get_victor) ^ ";\n # squares selected="
  ^ (state |> unselected |> string_of_bool)
  ^ "}"

(* let rec print_move (move : Game.State.move) = match move with | _ ->
   print_state (move state) *)

let game_over_test
    (name : string)
    (state : Game.State.t)
    (exp_out : bool) : test =
  name >:: fun _ ->
  assert_equal exp_out (game_over state) ~printer:string_of_bool

let victor_test
    (name : string)
    (state : Game.State.t)
    (exp_out : string) : test =
  name >:: fun _ -> assert_equal exp_out (get_victor state)

let player_test (name : string) (state : Game.State.t) (exp_out : int) :
    test =
  name >:: fun _ -> assert_equal exp_out (get_player state)

let unselected_test
    (name : string)
    (state : Game.State.t)
    (exp_out : bool) : test =
  name >:: fun _ -> assert_equal exp_out (unselected state)

let selected_test
    (name : string)
    (state : Game.State.t)
    (exp_out : int * int) : test =
  name >:: fun _ -> assert_equal exp_out (selected state)

let suc = List.sort_uniq compare

let getter_test
    (name : string)
    (state : Game.State.t)
    (getter : Game.State.t -> (int * int) list)
    (exp_out : (int * int) list) : test =
  name >:: fun _ -> assert_equal (suc exp_out) (getter state |> suc)

let if_mc_test (name : string) (state : Game.State.t) (exp_out : bool) :
    test =
  name >:: fun _ -> assert_equal exp_out (get_if_mc state)

let new_state (coord : int * int) (state : Game.State.t) : Game.State.t
    =
  match update state coord with
  | Continue s -> s
  | Legal s -> s
  | Illegal s -> s

let assert_continue (move : Game.State.turn) : bool =
  match move with
  | Continue s -> true
  | _ -> false

let assert_legal (move : Game.State.turn) : bool =
  match move with
  | Legal s -> true
  | _ -> false

let assert_illegal (move : Game.State.turn) : bool =
  match move with
  | Illegal s -> true
  | _ -> false

let update_test
    (name : string)
    (state : Game.State.t)
    (coord : int * int)
    (assert_move : turn -> bool) : test =
  name >:: fun _ ->
  assert_equal true
    (update state coord |> assert_move)
    ~printer:string_of_bool

let initial_state = init_state def_bd
let selected_state = new_state (3, 3) initial_state
let move_state = new_state (4, 4) selected_state

let cap_state =
  move_state |> new_state (6, 6) |> new_state (5, 5) |> new_state (4, 4)

let state_tests =
  [
    (* Initial State.*)
    game_over_test "Init State: game_over" initial_state false;
    victor_test "Init State: victor" initial_state "";
    player_test "Init State: player nunber" initial_state 1;
    unselected_test "Init State: unselected" initial_state true;
    selected_test "Init State: selected" initial_state (-1, -1);
    getter_test "Init State: moves" initial_state get_moves [];
    getter_test "Init State: captures" initial_state get_caps [];
    update_test "Init State: select (3, 3) is continue" initial_state
      (3, 3) assert_continue;
    (* Selected Board.*)
    game_over_test "Sel State: game_over" selected_state false;
    victor_test "Sel State: victor" selected_state "";
    player_test "Sel State: player nunber" selected_state 1;
    unselected_test "Sel State: unselected" selected_state false;
    selected_test "Sel State: selected" selected_state (3, 3);
    getter_test "Sel State: moves" selected_state get_moves
      [ (2, 4); (4, 4) ];
    getter_test "Sel State: captures" selected_state get_caps [];
    update_test "Sel State: move to (4, 4) is legal" selected_state
      (4, 4) assert_legal
    (* Moved Piece (3, 3) to (4, 4)*);
    game_over_test "P2 State: game_over" move_state false;
    victor_test "P2 State: victor" move_state "";
    player_test "P2 State: player nunber" move_state 2;
    unselected_test "P2 State: unselected" move_state true;
    selected_test "P2 State: selected" move_state (-1, -1);
    getter_test "P2 State: moves" move_state get_moves [];
    getter_test "P2 State: captures" move_state get_caps [];
    update_test "P2 State: select (6, 6) is continue" move_state (6, 6)
      assert_continue;
    (* P1 can capture.*)
    game_over_test "P1 Capture State: game_over" cap_state false;
    victor_test "P1 Capture State: victor" cap_state "";
    player_test "P1 Capture State: player nunber" cap_state 1;
    unselected_test "P1 Capture State: unselected" cap_state false;
    selected_test "P1 Capture State: selected" cap_state (4, 4);
    getter_test "P1 Capture State: moves" cap_state get_moves [ (3, 5) ];
    getter_test "P1 Capture State: captures" cap_state get_caps
      [ (6, 6) ];
    update_test "P1 Capture State: select (6, 6) is legal" cap_state
      (6, 6) assert_legal;
  ]

(* Add helper functions for testing Canvas here.*)
let canvas_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten [ board_tests; state_tests; canvas_tests ]

let _ = run_test_tt_main suite
