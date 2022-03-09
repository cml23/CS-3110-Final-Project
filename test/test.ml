open OUnit2
open Game.Command
open Game.State

(* TODO: add unit tests for modules below. You are free to reorganize
   the definitions below. Just keep it clear which tests are for which
   modules. *)

(* Add helper functions for testing Board here.*)
let board_tests = []

(* Add helper functions for testing State here.*)
let state_tests = []

(* Add helper functions for testing Canvas here.*)
let canvas_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten [ board_tests; state_tests; canvas_tests ]

let _ = run_test_tt_main suite
