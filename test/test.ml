open OUnit2
open Game.Ai
open Game.State
open Game.Board
open Game.Canvas
open Yojson

(* TESTING PHILOSPHY: BLACK BOX & PLAY TESTING *)
(* Due to the impossibility in testing every possible attribute in a
   given board or state, the difficulty of white box testing in the
   Board and State modules, and the graphical nature of the Canvas
   module - our team has opted for black box and play testing.*)
(* The Board and State modules were subject to both black box and
   playtesting. Canvas was playtested. *)

(* Add helper functions for testing Board here.*)

let string_of_tuple = function
  | x, y -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let rec string_of_tuple_list (selected : (int * int) list) =
  "[" ^ String.concat "; " (List.map string_of_tuple selected) ^ "]"

let string_of_tuple_opt (p : (int * int) option) =
  match p with
  | None -> "None"
  | Some (x, y) -> string_of_tuple (x, y)

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
  ^ string_of_tuple_list (fst lst)
  ^ "\n" ^ "Captured pieces: "
  ^ print_pc_lst (snd lst)

let def_bd_json = Yojson.Basic.from_file "data/default_board.json"
let def_bd = Game.Board.from_json def_bd_json

let four_by_ten_bd_json =
  Yojson.Basic.from_file "data/four_by_ten_board.json"

let four_by_ten_bd = Game.Board.from_json four_by_ten_bd_json

let six_by_six_bd_json =
  Yojson.Basic.from_file "data/six_by_six_board.json"

let six_by_six_bd = Game.Board.from_json six_by_six_bd_json
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
  assert_equal exp_out (xy_of_pc b pc) ~printer:string_of_tuple_opt

let neigbor_test
    (name : string)
    adj_fun
    (b : Game.Board.t)
    (x : int)
    (y : int)
    (exp_out : (int * int) option) : test =
  name >:: fun _ ->
  assert_equal exp_out (adj_fun b x y) ~printer:string_of_tuple_opt

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
  assert_equal exp_out (poss_moves b pc) ~printer:string_of_tuple_list

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

let json_test (name : string) (json : Yojson.Basic.t) =
  name >:: fun _ ->
  assert_equal json (Game.Board.to_json (Game.Board.from_json json))

let board_tests =
  [
    dim_x_test "default board x-dim" def_bd 8;
    dim_y_test "default board y-dim" def_bd 8;
    dim_x_test "four_by_ten board x-dim" four_by_ten_bd 4;
    dim_y_test "four_by_ten_bd board y-dim" four_by_ten_bd 10;
    dim_x_test "six_by_six_bd board x-dim" six_by_six_bd 6;
    dim_y_test "six_by_six_bd board y-dim" six_by_six_bd 6;
    piece_of_xy_test "default board at 1,1" def_bd 1 1
      (Some { player = 1; id = 1; is_royal = false });
    piece_of_xy_test "default board at 6,2" def_bd 6 2
      (Some { player = 1; id = 7; is_royal = false });
    piece_of_xy_test "default board at 8,8" def_bd 8 8
      (Some { player = 2; id = 24; is_royal = false });
    piece_of_xy_test "default board at 6,4" def_bd 6 4 None;
    piece_of_xy_test "four_by_ten_bd at 1,1" four_by_ten_bd 1 1
      (Some { player = 1; id = 1; is_royal = false });
    piece_of_xy_test "four_by_ten_bd at 3,9" four_by_ten_bd 3 9
      (Some { player = 2; id = 10; is_royal = false });
    piece_of_xy_test "four_by_ten_bd at 4,10" four_by_ten_bd 4 10
      (Some { player = 2; id = 12; is_royal = false });
    piece_of_xy_test "four_by_ten_bd at 2,4" four_by_ten_bd 2 4 None;
    piece_of_xy_test "six_by_six_bd at 1,1" six_by_six_bd 1 1
      (Some { player = 1; id = 1; is_royal = false });
    piece_of_xy_test "six_by_six_bd at 4,2" six_by_six_bd 4 2
      (Some { player = 1; id = 5; is_royal = false });
    piece_of_xy_test "six_by_six_bd at 6,6" six_by_six_bd 6 6
      (Some { player = 2; id = 12; is_royal = false });
    piece_of_xy_test "six_by_six_bd at 4,3" six_by_six_bd 4 3 None;
    num_pcs_of_pl_test "default board, #pcs for pl1" def_bd 1 12;
    num_pcs_of_pl_test "default board, #pcs for pl2" def_bd 2 12;
    num_pcs_of_pl_test "four_by_ten_bd, #pcs for pl1" four_by_ten_bd 1 6;
    num_pcs_of_pl_test "four_by_ten_bd, #pcs for pl2" four_by_ten_bd 2 6;
    num_pcs_of_pl_test "six_by_six_bd, #pcs for pl1" six_by_six_bd 1 6;
    num_pcs_of_pl_test "six_by_six_bd, #pcs for pl2" six_by_six_bd 2 6;
    xy_of_pc_test "default board; piece at 1,1" def_bd
      { player = 1; id = 1; is_royal = false }
      (Some (1, 1));
    xy_of_pc_test "default board; piece at 8,8" def_bd
      { player = 2; id = 24; is_royal = false }
      (Some (8, 8));
    xy_of_pc_test "default board; nonexistent pc" def_bd
      { player = 1; id = 13; is_royal = false }
      None;
    xy_of_pc_test "four_by_ten_bd; piece at 1,1" four_by_ten_bd
      { player = 1; id = 1; is_royal = false }
      (Some (1, 1));
    xy_of_pc_test "four_by_ten_bd; piece at 4,10" four_by_ten_bd
      { player = 2; id = 12; is_royal = false }
      (Some (4, 10));
    xy_of_pc_test "four_by_ten_bd; nonexistent pc" four_by_ten_bd
      { player = 1; id = 7; is_royal = false }
      None;
    xy_of_pc_test "six_by_six_bd; piece at 1,1" six_by_six_bd
      { player = 1; id = 1; is_royal = false }
      (Some (1, 1));
    xy_of_pc_test "six_by_six_bd; piece at 6,6" six_by_six_bd
      { player = 2; id = 12; is_royal = false }
      (Some (6, 6));
    xy_of_pc_test "six_by_six_bd; nonexistent pc" six_by_six_bd
      { player = 1; id = 7; is_royal = false }
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
    json_test "check correct to/from json parsing for def_bd"
      def_bd_json;
    json_test "check correct to/from json parsing for four_by_ten_bd"
      four_by_ten_bd_json;
    json_test "check correct to/from json parsing for six_by_six_bd"
      six_by_six_bd_json;
  ]

(* Add helper functions for testing State here.*)

let print_state (state : Game.State.t) =
  String.concat ""
    [
      "{game_over=";
      state |> game_over |> string_of_bool;
      ";\n victor=";
      string_of_int (state |> get_vc);
      ";\n # squares selected=";
      state |> unselected |> string_of_bool;
      "}";
    ]

let string_of_move (mv : Game.State.move) =
  String.concat ""
    [
      "{";
      "s: ";
      string_of_tuple mv.start;
      ", ";
      "f: ";
      string_of_tuple mv.finish;
      "}";
    ]

let print_urdos (mvs : move list) =
  "[ " ^ String.concat "; " (List.map string_of_move mvs) ^ " ]"
(* let rec print_move (move : Game.State.move) = match move with | _ ->
   print_state (move state) *)

let game_over_test
    (name : string)
    (state : Game.State.t)
    (exp_out : bool) : test =
  name >:: fun _ ->
  assert_equal exp_out (game_over state) ~printer:string_of_bool

let int_test f (name : string) (state : Game.State.t) (exp_out : int) :
    test =
  name >:: fun _ -> state |> f |> assert_equal exp_out

let unselected_test
    (name : string)
    (state : Game.State.t)
    (exp_out : bool) : test =
  name >:: fun _ ->
  assert_equal exp_out (unselected state) ~printer:string_of_bool

let selected_test
    (name : string)
    (state : Game.State.t)
    (exp_out : int * int) : test =
  name >:: fun _ ->
  assert_equal exp_out (selected state) ~printer:string_of_tuple

let suc = List.sort_uniq compare

let getter_test
    (name : string)
    (state : Game.State.t)
    (getter : Game.State.t -> (int * int) list)
    (exp_out : (int * int) list) : test =
  name >:: fun _ ->
  assert_equal (suc exp_out)
    (getter state |> suc)
    ~printer:string_of_tuple_list

let if_mc_test (name : string) (state : Game.State.t) (exp_out : bool) :
    test =
  name >:: fun _ -> assert_equal exp_out (get_if_mc state)

let new_state (coord : int * int) (state : Game.State.t) : Game.State.t
    =
  update coord state |> get_state

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
    (update coord state |> assert_move)
    ~printer:string_of_bool

let urdo_len_test
    (name : string)
    (state : Game.State.t)
    (f : Game.State.t -> move list)
    (exp_out : int) : test =
  name >:: fun _ ->
  assert_equal exp_out
    (state |> f |> List.length)
    ~printer:string_of_int

let urdo_test
    (name : string)
    (state : Game.State.t)
    (f : Game.State.t -> move list)
    (exp_out : Game.State.move) : test =
  name >:: fun _ ->
  assert_equal exp_out (state |> f |> List.hd) ~printer:string_of_move

(*=========BASIC TESTS=========*)
let is1 = init_state 1 def_bd
let selected_state = new_state (3, 3) is1
let move_state = new_state (4, 4) selected_state

(* TODO: Test move indirectly *)

let basic_tests =
  [
    (* Initial State.*)
    game_over_test "Init State: game_over" is1 false;
    int_test get_vc "Init State: victor" is1 0;
    int_test get_player "Init State: player nunber" is1 1;
    unselected_test "Init State: unselected" is1 true;
    selected_test "Init State: selected" is1 (-1, -1);
    getter_test "Init State: moves" is1 get_moves [];
    getter_test "Init State: captures" is1 get_caps [];
    update_test "Init State: select (3, 3) is continue" is1 (3, 3)
      assert_continue;
    (* Selected Board.*)
    game_over_test "Sel State: game_over" selected_state false;
    int_test get_vc "Sel State: victor" selected_state 0;
    int_test get_player "Sel State: player nunber" selected_state 1;
    unselected_test "Sel State: unselected" selected_state false;
    selected_test "Sel State: selected" selected_state (3, 3);
    getter_test "Sel State: moves" selected_state get_moves
      [ (2, 4); (4, 4) ];
    getter_test "Sel State: captures" selected_state get_caps [];
    update_test "Sel State: reselect (1, 1) is continue" selected_state
      (1, 1) assert_continue;
    update_test "Sel State: move to (3, 4) is illegal" selected_state
      (3, 4) assert_illegal;
    update_test "Sel State: move to (4, 4) is legal" selected_state
      (4, 4) assert_legal
    (* Moved Piece (3, 3) to (4, 4)*);
    urdo_len_test "P2 State: 1 undo possible" move_state
      Game.State.get_undos 1;
    (* urdo_test "P2 State: Undo is {s: (3, 3), f: (4, 4)}" move_state
       Game.State.get_undos m1; *)
    game_over_test "P2 State: game_over" move_state false;
    int_test get_vc "P2 State: victor" move_state 0;
    int_test get_player "P2 State: player nunber" move_state 2;
    unselected_test "P2 State: unselected" move_state true;
    selected_test "P2 State: selected" move_state (-1, -1);
    getter_test "P2 State: moves" move_state get_moves [];
    getter_test "P2 State: captures" move_state get_caps [];
    update_test "P2 State: select (6, 6) is continue" move_state (6, 6)
      assert_continue;
  ]

(*=========CAPTURE TESTS=========*)

let apply_coord (st : Game.State.t) (coord : int * int) : Game.State.t =
  st |> new_state coord

let coord_applier (coords : (int * int) list) (st : Game.State.t) =
  List.fold_left apply_coord st coords

(* Create new state to prevent Stack mutability from crossing to new
   test cases.*)
let cap_coords = [ (3, 3); (4, 4); (6, 6); (5, 5); (4, 4) ]
let cap_state = is1 |> coord_applier cap_coords

let cap_tests =
  [
    (* P1 can capture.*)
    game_over_test "P1 Capture State: game_over" cap_state false;
    int_test get_vc "P1 Capture State: victor" cap_state 0;
    int_test get_player "P1 Capture State: player nunber" cap_state 1;
    unselected_test "P1 Capture State: unselected" cap_state false;
    selected_test "P1 Capture State: selected" cap_state (4, 4);
    getter_test "P1 Capture State: moves" cap_state get_moves [ (3, 5) ];
    getter_test "P1 Capture State: captures" cap_state get_caps
      [ (6, 6) ];
    urdo_len_test "P1 Capture State: 2 undo possible" cap_state
      Game.State.get_undos 2;
    (* urdo_test "P1 Capture State: Undo is {s: (6, 6), f: (5, 5)}"
       cap_state Game.State.get_undos m2; *)
    update_test "P1 Capture State: select (6, 6) is legal" cap_state
      (6, 6) assert_legal;
  ]

(*=========MULTICAPTURE TESTS=========*)

let mc_cords =
  [
    (3, 3);
    (2, 4);
    (2, 6);
    (1, 5);
    (2, 4);
    (3, 5);
    (3, 7);
    (2, 6);
    (4, 2);
    (3, 3);
    (4, 6);
    (2, 4);
  ]

let mc_state = is1 |> coord_applier mc_cords
let mc_state2 = is1 |> coord_applier mc_cords |> new_state (2, 4)

let mc_tests =
  [
    (* P2 has just captured, another capture is available.*)
    game_over_test "P2 MC State: game_over" mc_state false;
    int_test get_vc "P2 MC State: victor" mc_state 0;
    int_test get_player "P2 MC State: player number" mc_state 2;
    unselected_test "P2 MC State: unselected" mc_state true;
    selected_test "P2 MC State: selected" mc_state (-1, -1);
    getter_test "P2 MC State: moves" mc_state get_moves [];
    getter_test "P2 MC State: captures" mc_state get_caps [ (4, 2) ];
    urdo_len_test "P2 MC State: 2 undo possible" mc_state
      Game.State.get_undos 6;
    (* Testing move is impossible due to unknown piece. *)
    if_mc_test "P2 MC State: state.mc is true" mc_state true;
    update_test "P2 MC State: select (2, 4) is continue" mc_state (2, 4)
      assert_continue;
    update_test "P2 MC State: select (4, 2) is legal" mc_state2 (4, 2)
      assert_legal;
  ]

(*=========UNDO & REDO TESTS=========*)
let u1_state = mc_state2 |> urdo true |> get_state
let u2_state = u1_state |> urdo true |> get_state
let u3_state = u2_state |> urdo true |> get_state
let r1_state = u2_state |> urdo false |> get_state

let urdo_tests =
  [
    int_test get_player "P2 Undo MC State: player number" u1_state 2;
    unselected_test "P2 MC State: unselected" u1_state true;
    selected_test "P2 MC State: selected" u1_state (-1, -1);
    getter_test "P2 MC State: moves" u1_state get_moves [];
    getter_test "P2 MC State: captures" u1_state get_caps [];
    urdo_len_test "P2 MC State: 2 undo possible" u1_state
      Game.State.get_undos 5;
    int_test get_player "P1 Undo MC State: player number" u2_state 2;
    unselected_test "P1 MC State: unselected" u2_state true;
    selected_test "P1 MC State: selected" u2_state (-1, -1);
    getter_test "P1 MC State: moves" u2_state get_moves [];
    getter_test "P1 MC State: captures" u2_state get_caps [];
    urdo_len_test "P1 MC State: 2 undo possible" u2_state
      Game.State.get_undos 4;
    int_test get_player "P2 Undo MC State: player number" u1_state 2;
    unselected_test "P2 MC State: unselected" mc_state true;
    selected_test "P2 MC State: selected" mc_state (-1, -1);
    getter_test "P2 MC State: moves" mc_state get_moves [];
    getter_test "P2 MC State: captures" mc_state get_caps [ (4, 2) ];
    urdo_len_test "P2 MC State: 2 undo possible" mc_state
      Game.State.get_undos 6;
  ]

(*=========VICTOR TESTS=========*)

(* Add helper functions for testing Canvas here.*)
let canvas_tests = [ (*More like Canvas tests me amirite?*) ]

let suite =
  "test suite for A2"
  >::: List.flatten
         [
           board_tests;
           basic_tests;
           cap_tests;
           mc_tests;
           urdo_tests;
           canvas_tests;
         ]

let _ = run_test_tt_main suite
