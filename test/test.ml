open OUnit2
open Game.Command
open Game.State
open Game.Board
open Game.Canvas

(* TODO: add unit tests for modules below. You are free to reorganize
   the definitions below. Just keep it clear which tests are for which
   modules. *)

(* Add helper functions for testing Board here.*)

let print_int_pair_opt (p : (int * int) option) =
  match p with
  | None -> "None"
  | Some (x, y) -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

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
  ]

(* Add helper functions for testing State here.*)
let state_tests = []

(* Add helper functions for testing Canvas here.*)
let canvas_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten [ board_tests; state_tests; canvas_tests ]

let _ = run_test_tt_main suite
