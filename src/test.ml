open Interp
open Main
open Ast
open OUnit2

let make_helper string_of_val name i o  =
  name >:: (fun _ -> assert_equal (string_of_val i) (interp o))

let make_i = make_helper string_of_int
let make_b = make_helper string_of_bool

let tests_add_mult = [
  make_i "int" 22 "22";
  make_i "add" 22 "11+11";
  make_i "add three" 22 "11+5+6";
  make_i "mult two" 22 "2*11";
  make_i "mult three" 22 "1*2*11";
  make_i "mult negatives" 22 "-1*-22";
  make_i "mult on right of add" 22 "2+2*10";
  make_i "mult on left of add" 22 "2*1+20";
  make_i "nested mult on right of add" 64 "4+2*(15*2)"
]

let tests_leq = [
  make_b "true <=" true "5 <= 7";
  make_b "false <=" false "5 <= -1"
]

let tests = List.flatten [
  tests_add_mult; tests_leq
]

let _ = run_test_tt_main ("suite" >::: tests)