open Interp
open Main
open Ast
open OUnit2

let make_helper string_of_val name i o  =
  [name >:: (fun _ -> assert_equal (string_of_val i) (interp o));
   name >:: (fun _ -> assert_equal (string_of_val i) (interp_big o))]

let make_i = make_helper string_of_int
let make_b = make_helper string_of_bool

let make_t n i o =
  [n >:: (fun _ -> assert_raises (Failure i) (fun () -> interp o));
  n >:: (fun _ -> assert_raises (Failure i) (fun () -> interp_big o))]

let tests_add_mult = [
  make_i "int" 22 "22";
  make_i "add" 22 "11+11";
  make_i "add three" 22 "11+5+6";
  make_i "mult two" 22 "2*11";
  make_i "mult three" 22 "1*2*11";
  make_i "mult negatives" 22 "-1*-22";
  make_i "mult on right of add" 22 "2+2*10";
  make_i "mult on left of add" 22 "2*1+20";
  make_i "nested mult on right of add" 64 "4+2*(15*2)";
  make_i "adds" 22 "(10+1)+(5+6)";
  make_i "mul2" 22 "2+2*10";
  make_i "mul3" 14 "2*2+10";
  make_i "mul4" 40 "2*2*10"
]

let tests_leq = [
  make_b "true <=" true "5 <= 7";
  make_b "false <=" false "5 <= -1"
]

let tests_let = [
  make_i "variable shadowing" 11 "let x = 5 in ((let x = 6 in x) + x)";
  make_i "variable shadowing complex" 4 "let x = 1 in (let x = x + x in x + x)";
  make_b "boolean binding" true "let x = 5 in (let x = 7 in x <= x)";
  make_i "let" 22 "let x=22 in x";
  make_i "lets" 22 "let x = 0 in let x = 22 in x"
]

let tests_if = [
  make_i "if1" 22 "if true then 22 else 0";
  make_b "true" true "true";
  make_i "if2" 22 "if 1+2 <= 3+4 then 22 else 0";  
  make_i "if3" 22 "if 1+2 <= 3*4 then let x = 22 in x else 0";
  make_i "letif" 22 "let x = 1+2 <= 3*4 in if x then 22 else 0"
]

let tests_err = [
  make_t "ty plus" bop_err "1 + true";
  make_t "ty mult" bop_err "1 * false";
  make_t "ty leq" bop_err "true <= 1";
  make_t "if guard" if_guard_err "if 1 then 2 else 3";
  make_t "unbound" unbound_var_err "x"
]

let tests = List.flatten [
  tests_add_mult; tests_leq; tests_let; tests_if; tests_err
]

let _ = run_test_tt_main ("suite" >::: List.flatten tests)