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

let print_pc_opt pc_opt =
  match pc_opt with
  | None -> "None"
  | Some pc -> print_pc pc

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
      { player = 2; id = 17; is_royal = false }
      [ (3, 5); (1, 5) ];
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
  ^ (state |> num_selected |> string_of_int)
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

let selected_test
    (name : string)
    (state : Game.State.t)
    (exp_out : (int * int) list) : test =
  name >:: fun _ -> assert_equal exp_out (selected state)

let update_test
    (name : string)
    (state : Game.State.t)
    (exp_out : (int * int) list) : test =
  name >:: fun _ -> assert_equal exp_out (selected state)

let def_st = init_state def_bd

let state_tests =
  [
    game_over_test "initial state game_over" def_st false;
    victor_test "initial state game_over" def_st "";
    selected_test "initial state game_over" def_st []
    (* update_test "initial state game_over" def_st false; *);
  ]

(* Add helper functions for testing Canvas here.*)
let canvas_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten [ board_tests; state_tests; canvas_tests ]

let _ = run_test_tt_main suite
