open Ast

(**[Env] is a module that helps with making maps which have strings as keys. *)
module Env = Map.Make(String)

let empty_env = Env.empty

(**[env] is an environment which maps a string to a value. *)
type env = value Env.t
and value = 
  | VInt of int
  | VBool of bool


(** The error message produced if a variable is unbound. *)
let unbound_var_err = "Unbound variable"

(** The error message produced if binary operators and their
    operands do not have the correct types. *)
let bop_err = "Operator and operand type mismatch"

(** The error message produced if the [then] and [else] branches
    of an [if] do not have the same type. *)
let if_branch_err = "Branches of if must have same type"

(** The error message produced if the guard
    of an [if] does not have type [bool]. *)
let if_guard_err = "Guard of if must have type bool"

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let e = Parser.prog Lexer.read lexbuf in
  e

(**[string_of_val x] is [x] represented as a string. *)
let string_of_val x =
  match x with
  | Int a -> string_of_int a
  | Bool true -> string_of_bool true
  | Bool false -> string_of_bool false
  | Var _ | Binop _ | Let _ | If _-> failwith "cannot convert this expr to string"

(**[is_value e] is whether [e] is a value. *)
let is_value = function
  | Int _ | Bool _ -> true
  | Var _ | Binop _ | Let _ | If _ -> false


(**[subst e2 v x] is [e2{v/x}]. Basically e2 with v subsituted for x in it.*)
let rec subst e2 v x =
  match e2 with
  | Int _ | Bool _ -> e2
  | Var a -> if x = a then v else e2
  | Binop (bop, e', e'') -> Binop (bop, subst e' v x, subst e'' v x)
  | If (e', e'', e''') -> If (subst e' v x, subst e'' v x, subst e''' v x)
  | Let (st, e', e'') -> 
    if st = x then Let (st, subst e' v x, e'') 
    else Let (st, subst e' v x, subst e'' v x)


(**[step] is a SINGLE step of evaluation. *)
let rec step e = 
  match e with
  | Int _ | Bool _ -> e
  | Var _ -> failwith unbound_var_err
  | Let (x, e1, e2) -> (
    match is_value e1, is_value e2 with
    | false, _ -> step (Let (x, step e1, e2))
    | true, false -> step (subst e2 e1 x)
    | true, true -> e2
  )
  | Binop (x, e1, e2) -> (
    match is_value e1, is_value e2 with
    | false, _ -> step (Binop (x, step e1, e2))
    | true, false -> step (Binop (x, e1, step e2))
    | true, true -> step_bop x e1 e2
  )
  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (Int _, _, _) -> failwith if_guard_err
  | If (e1, e2, e3) -> step (If (step e1, e2, e3))
and 
step_bop bop e1 e2 = 
  match bop, e1, e2 with
  | Add, Int x, Int y -> Int (x + y)
  | Add, _ , _ -> failwith bop_err
  | Mult, Int x, Int y -> Int (x * y)
  | Mult, _, _ -> failwith bop_err
  | Leq, Int x, Int y -> Bool (x <= y)
  | Leq, _, _ -> failwith bop_err


(**[eval e] evaluates [e] completely to some value [v].*)
let rec eval e =
  if is_value e then e else eval (step e)


(** [eval_big e] is the big evaluation [e ==> v] relation. *)
let rec eval_big e =
  match e with
  | Int _ | Bool _ -> e
  | Var _ -> failwith unbound_var_err
  | Let (x, e1, e2) -> subst e2 (eval_big e1) x |> eval_big
  | Binop (x, e1, e2) -> eval_bop x e1 e2
  | If (e1, e2, e3) -> eval_if e1 e2 e3

and eval_bop bop e1 e2 = 
  match bop, eval_big e1, eval_big e2 with
  | Add, Int x, Int y -> Int (x + y)
  | Mult, Int x, Int y -> Int (x * y)
  | Leq, Int x, Int y -> Bool (x <= y)
  | _ -> failwith bop_err
  
and eval_if e1 e2 e3 =
  match eval_big e1 with
  | Bool true -> eval_big e2
  | Bool false -> eval_big e3
  | Int _ | Var _  | Binop _ | Let _ | If _ -> failwith if_guard_err


(**[eval_env env e] big evaluates [e] to a value [v] using the environment model. *)
let rec eval_env (env : env) (e : expr) : value =
  match e with
  | Var x -> eval_var env x
  | Int a -> VInt a
  | Bool b -> VBool b
  | Binop (x, e1, e2) -> eval_env_bop env x e1 e2
  | If (e1, e2, e3) -> eval_env_if env e1 e2 e3
  | Let (x, e1, e2) -> eval_env_let env x e1 e2
and eval_env_let env x e1 e2 =
  let v = eval_env env e1 in
  let env' = Env.(add x v env) in
  eval_env env' e2

and eval_env_if env e1 e2 e3 =
  match eval_env env e1 with
  | VBool true -> eval_env env e2
  | VBool false -> eval_env env e3
  | VInt _ -> failwith if_guard_err

and eval_env_bop env x e1 e2 =
  match x, eval_env env e1, eval_env env e2 with
  | Add, VInt x, VInt y -> VInt (x + y)
  | Mult, VInt x, VInt y -> VInt (x * y)
  | Leq, VInt x, VInt y -> VBool (x <= y)
  | _ -> failwith bop_err

and eval_var env x =
 match Env.find_opt x env with
 | Some v -> v
 | None -> failwith unbound_var_err


(**[extract_val e] represents value [e] as an expr.*)
let extract_val e =
  match e with
  | VInt x -> Int x
  | VBool y -> Bool y



(**[interp s] evaluates [s] to [v] and returns [v] in string format. *)
let interp s = 
  s |> parse |> eval |> string_of_val

let interp_eval s =
  s |> parse |> eval_env empty_env |> extract_val |> string_of_val

let interp_big = interp_eval

