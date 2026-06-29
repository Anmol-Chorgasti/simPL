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

let type_err = "Constraint set cannot be unified due to type error"


(**[const] represents a pair of typs. IT implies that if (t1,t2) is a constraint, then t1 = t2. *)
type const = typ * typ
type const_set = const list

let counter = ref 0
let fresh_var () = 
  counter := !counter + 1;
  "a"^(string_of_int !counter)


type tscheme = Gen of (string list * typ) | Ins of typ
module SEnv = Map.Make(String)
type env = tscheme SEnv.t
let empty = SEnv.empty

module InstMap = struct
  type t = (string * string) list
  let empty_mp = []
  let find = List.assoc_opt
  let rem  = List.remove_assoc
  let add key data xs = (key, data) :: (rem key xs)
  let inst_map xs = 
    List.map (fun x -> (x, fresh_var ())) xs
end

(**[substitute e t1 t2] is a type with all occurences of [t1] in [e] replaced with [t2].*)
let rec substitute (e:typ) (tv:string) (t2:typ) : typ =
  match e with
  | TInt | TBool -> e
  | TVar a -> if a = tv then t2 else e
  | TFun (e1, e2) -> TFun (substitute e1 tv t2, substitute e2 tv t2)


(**[instantiate xs t] replaces is t with all occurences of variables in [xs] in [t]
   replaced with new fresh variables *)
let instantiate xs t = 
  let open InstMap in
  let imap = inst_map xs in
  let get_rep x = 
    match find x imap with
    | None -> TVar x
    | Some y -> TVar y
  in
  let rec aux t=
    match t with
    | TInt | TBool -> t
    | TVar a -> get_rep a
    | TFun (t1, t2) -> TFun (aux t1, aux t2)
  in
  aux t

let generalize (env:env) (t:typ) : tscheme =
  let rec aux t =
    match t with
    | TVar a -> [a]
    | TInt | TBool -> []
    | TFun (t1, t2) -> (aux t1) |> List.rev_append (aux t2)
  in
  let fvars xs t =
    let vars = aux t in
    List.filter (fun x -> not (List.mem x xs)) vars
  in
  let vars_env = 
    SEnv.fold 
    (
      fun _ v acc -> 
        match v with
        | Ins t1  -> List.rev_append (aux t1) acc
        | Gen (xs, t1) -> List.rev_append (fvars xs t1) acc 
        (* only need free vars from gen type scheme, local variables are not necessary. *)
    ) env [] 
  in
  let tvars_only = fvars vars_env t in
  Gen (List.sort_uniq compare tvars_only, t)


(*UNIFICATION WORK*)
type submap = (string * typ) list

(**[occurs e x] is true if type variable [x] is used in the [e] type and false if not. *)
let rec occurs (e: typ) (x: string) : bool =
  match e with
  | TVar a -> x = a
  | TInt | TBool -> false
  | TFun (e1, e2) -> (occurs e1 x) || (occurs e2 x)



(**[sub_const x tr ct] is a new constraint with type [tr] substituted for type variable [x]*)
let sub_const (x:string) (tr:typ) (ct:const) : const =
    match ct with
    | t1, t2 -> (substitute t1 x tr, substitute t2 x tr)



(**[sub_map x tr s] is the (key, value) pair [s] with type var x replaced by [tr] in value. *)  
let sub_map (x:string) (tr:typ) (s:string * typ) : string * typ =
  match s with
  | k, t -> k, substitute t x tr



(**[update_cs x tr] is a constraint list with all constraints in [cs] having x replaced by [tr].*)
let update_cs cs x tr = List.map (sub_const x tr) cs



(**[update_acc acc x tr] is a list of substitutions [acc] with
  1. A new substituion of replacing type var [x] with [tr]
  2. All other mentions of [x] replaced with [tr] in [acc]. *)
let update_acc acc x tr = 
  (*first replace all occurences of TVar x with tr in existing acc*)
  let nacc = List.map (sub_map x tr) acc in
  (x,tr)::nacc



(**[sub_solution mp t] is a type with all substitutions in [mp] applied in order to [t]*)
let rec sub_solution (mp:submap) (t:typ) : typ = 
  match mp with
  | [] -> t
  | (x, tr) :: rest -> substitute t x tr |> (sub_solution rest)



(**[sub_env mp env] is a static environment with all substitutions in [mp] applied in order to
   every key,value pair in [env]*)
let sub_env (mp:submap) (env:env) : env =
  let sub_aux_tscheme mp (t:tscheme) : tscheme =
    match t with
    | Ins ty -> Ins (sub_solution mp ty)
    | Gen (xs, ty) ->
      let new_mp = List.filter (fun (x,_) -> not (List.mem x xs)) mp in
      Gen (xs, sub_solution new_mp ty)
  in
  SEnv.map (fun t -> sub_aux_tscheme mp t) env



(**[unify cs] unifies constraint set [cs] and produces an ordered list of 
   substitutions made as the solution. 
   Raises: [Type error] if constraint set cannot be unified. *)
let rec unify ?(acc=[]) (cs:const_set) : submap = 
  match cs with
  | [] -> List.rev acc
  | (t1, t2) :: tl -> 
    match t1, t2 with
    | TInt, TBool | TBool, TInt -> failwith type_err
    | TFun _, (TBool | TInt) | (TBool | TInt), TFun _ -> failwith type_err
    | TInt, TInt | TBool, TBool -> unify ~acc:acc tl
    | TFun (t1, t2) , TFun (t3, t4) -> unify ~acc:acc ((t1,t3) :: (t2,t4) :: tl)
    | TVar x, TVar y ->
      if x=y then unify ~acc:acc tl
      else
      let new_tl = update_cs tl x (TVar y) in
      let new_acc = update_acc acc x (TVar y) in 
      (*a type variable x is updated with another y but as TVar y and not y only!*)
      unify ~acc:new_acc new_tl
    | TVar x, tr | tr, TVar x -> 
      if occurs tr x then failwith type_err
      else
      let new_tl = update_cs tl x tr in
      let new_acc = update_acc acc x tr in
      unify ~acc:new_acc new_tl



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
  | Let (x, e1, e2) -> let_helper env x e1 e2

and let_helper env x e1 e2 =
  let (e1_t, e1_c) = gen_consts env e1 in
  let e1_s = unify e1_c in
  let e1_u = sub_solution e1_s e1_t in
  let new_env = sub_env e1_s env in
  let gen_e1_u = generalize new_env e1_u in
  let final_env = SEnv.add x gen_e1_u new_env in
  gen_consts final_env e2

and app_helper env e1 e2 =
  let new_var = fresh_var () in
  let (e1_t, e1_c) = gen_consts env e1 in
  let (e2_t, e2_c) = gen_consts env e2 in
  let mc = List.rev_append e1_c e2_c in
  let new_c = (e1_t, TFun (e2_t, TVar new_var)) :: mc in
  (TVar new_var, new_c)
  
and fun_helper env x e2 = 
  let new_var = fresh_var () in
  let new_env = SEnv.add x (Ins (TVar new_var)) env in
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
  | Some v -> (
    match v with
    | Ins t1 -> (t1, [])
    | Gen (xs, t1) -> (instantiate xs t1, [])
  )

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