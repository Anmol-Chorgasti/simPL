open Ast

(** The error message produced if a variable is unbound. *)
let unbound_var_err = "Unbound variable"

(**The error message produced if binary operator used on non int types*)
let bop_err = "Binary operation on non integer values"

(** The error message produced if the [then] and [else] branches
    of an [if] do not have the same type. *)
let if_branch_err = "Branches of if must have same type"

(** The error message produced if the guard
    of an [if] does not have type [bool]. *)
let if_guard_err = "Guard of if must have type bool"

let fun_app_err = "Cannot apply non function value to expression"


(**[const] represents a pair of typs. IT implies that if (t1,t2) is a constraint, then t1 = t2. *)
type const = typ * typ
type const_set = const list

let counter = ref 0
let fresh_var () = 
  counter := !counter + 1;
  "a"^(string_of_int !counter)


module SEnv = Map.Make(String)
type env = typ SEnv.t
let empty = SEnv.empty


(**[gen_consts env e] is inferred type of [e] and all constraints that must 
   have a valid solution for [e] to type check. *)
let rec gen_consts (env : env) (e : expr) : (typ * const_set) =
  match e with
  | Int _ -> (TInt, [])
  | Bool _ -> (TBool, [])
  | Var x -> var_helper env x
  | Binop (bop, e1, e2)-> bop_helper env bop e1 e2
  | If (e1, e2, e3) -> if_helper env e1 e2 e3
  | Fun (x, e2) -> fun_helper env x e2
  | App (e1, e2) -> app_helper env e1 e2
  | Let _ -> failwith "todo"

and app_helper env e1 e2 =
  let new_var = fresh_var () in
  let (e1_t, e1_c) = gen_consts env e1 in
  let (e2_t, e2_c) = gen_consts env e2 in
  let mc = List.rev_append e1_c e2_c in
  let new_c = (e1_t, TFun (e2_t, TVar new_var)) :: mc in
  (TVar new_var, new_c)
  
and fun_helper env x e2 = 
  let new_var = fresh_var () in
  let new_env = SEnv.add x (TVar new_var) env in
  let (e2_t, e2_c) = gen_consts new_env e2 in
  (TFun (TVar new_var, e2_t),e2_c)

and if_helper env e1 e2 e3 =
  let (e1_t, e1_c) = gen_consts env e1 in
  let (e2_t, e2_c) = gen_consts env e2 in
  let (e3_t, e3_c) = gen_consts env e3 in
  let mc = List.rev_append e1_c e2_c |> List.rev_append e3_c
  in
  let new_var = fresh_var () in
  let new_c = [
    (e1_t, TBool);
    (TVar new_var, e2_t);
    (TVar new_var, e3_t)
  ] |> List.rev_append mc 
  in (TVar new_var, new_c)

and var_helper env x =
  match SEnv.find_opt x env with
  | None -> failwith (x^": "^unbound_var_err)
  | Some v -> (v,[])

and bop_helper env bop e1 e2 =
  let (e1_t, e1_c)  = gen_consts env e1 in
  let (e2_t, e2_c) = gen_consts env e2 in
  let mc = List.rev_append e1_c e2_c in
  let new_mc =  [
    (e1_t, TInt);
    (e2_t, TInt)
  ] |> List.rev_append mc
  in
  match bop with
  | Add | Mult -> (TInt, new_mc)
  | Leq -> (TBool, new_mc)




