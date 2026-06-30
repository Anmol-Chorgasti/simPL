open Ast
open Type_inference


(**[Env] is a module that helps with making maps which have strings as keys. *)
module Env = Map.Make(String)

let empty_env = Env.empty

(**[env] is an environment which maps a string to a value. *)
type env = value Env.t
and value = 
  | VInt of int
  | VBool of bool
  | VFun of string * expr * env

(**[parse s] is the representation of [s] as an Abstrat Syntax Tree. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let e = Parser.prog Lexer.read lexbuf in
  e

(**[string_of_val x] is [x] represented as a string. *)
let string_of_val x =
  match x with
  | VInt a -> string_of_int a
  | VBool true -> string_of_bool true
  | VBool false -> string_of_bool false
  | VFun _ -> "<func>"


(**[eval_env env e] big evaluates [e] to a value [v] using the environment model. *)
let rec eval_env (env : env) (e : expr) : value =
  match e with
  | Var x -> eval_var env x
  | Int a -> VInt a
  | Bool b -> VBool b
  | Binop (x, e1, e2) -> eval_env_bop env x e1 e2
  | If (e1, e2, e3) -> eval_env_if env e1 e2 e3
  | Let (x, e1, e2) -> eval_env_let env x e1 e2
  | Fun (x, e1) -> VFun (x, e1, env) (* cannot evaluate a value *)
  | App (e1, e2) -> eval_env_app env e1 e2

and eval_env_app env e1 e2 =
  match eval_env env e1 with
  | VFun (x, e, env_f) -> (
    let v = eval_env env e2 in
    let env_f' = Env.(add x v env_f) in
    eval_env env_f' e
  )
  | VBool _ | VInt _ -> failwith type_err

and eval_env_let env x e1 e2 =
  let v = eval_env env e1 in
  let env' = Env.(add x v env) in
  eval_env env' e2

and eval_env_if env e1 e2 e3 =
  match eval_env env e1 with
  | VBool true -> eval_env env e2
  | VBool false -> eval_env env e3
  | VInt _ | VFun _-> failwith type_err

and eval_env_bop env x e1 e2 =
  match x, eval_env env e1, eval_env env e2 with
  | Add, VInt x, VInt y -> VInt (x + y)
  | Mult, VInt x, VInt y -> VInt (x * y)
  | Leq, VInt x, VInt y -> VBool (x <= y)
  | _ -> failwith type_err

and eval_var env x =
 match Env.find_opt x env with
 | Some v -> v
 | None -> failwith unbound_var_err


(**[interp s] evaluates [s] to [v] and returns [v] in string format. *)
let interp s = 
  let s_exp = parse s in
  let _ = infer s_exp in
  s_exp |> eval_env empty_env |> string_of_val

(**[interp_w_typ s] infers both type of and evaluates [s].*)
let interp_w_typ s =
  let s_exp = parse s in
  let s_t = infer s_exp in
  let s_v = s_exp |> eval_env empty_env |> string_of_val in
  let () = "Output: -: " ^ s_t ^ " = " ^ s_v |> print_endline in ()
