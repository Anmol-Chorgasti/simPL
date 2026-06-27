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


module type StaticEnvironment = sig
  (**[t] is the type of a static environment*)
  type t

  (**[empty] is the empty static environment*)
  val empty : t

  (**[lookup env x] is the type bound to [x] in env.
     Raises: [Failure] if [x] not found in env. *)
  val lookup : t -> string -> typ

  (**[extend env x ty] is [env] extended with a binding of [x] to [ty].*)
  val extend : t -> string -> typ -> t
end

module StaticEnvironment : StaticEnvironment = struct
  type t = (string * typ) list
  let empty = []
  let lookup (env:t) (x:string) : typ = 
    match List.assoc_opt x env with
    | None -> failwith unbound_var_err
    | Some v -> v
  let extend (env:t) (x:string) (ty:typ) : t = (x,ty)::env
end

(**
(**[typeof env x] is the type of value [x] evaluates to.
   Raises : [Failure] if [x] does not type check *)
let rec typeof (env : StaticEnvironment.t) (x:expr) : typ =
  let open StaticEnvironment in
  match x with
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var z -> lookup env z
  | Binop (bop, e1, e2) -> binop_type_helper env bop e1 e2
  | Let (x, e1, e2) -> let_type_helper env x e1 e2
  | If (e1, e2, e3) -> if_type_helper env e1 e2 e3
and binop_type_helper env bop e1 e2 =
  match bop, typeof env e1, typeof env e2 with
  | (Add | Mult), TInt, TInt -> TInt
  | Leq, TInt, TInt -> TBool
  | _, TBool , _ | _, _, TBool -> failwith bop_err
and let_type_helper env x e1 e2 =
  let open StaticEnvironment in
  let v = typeof env e1 in
  let env' = extend env x v in
  typeof env' e2
and if_type_helper env e1 e2 e3 =
  match typeof env e1 with
  | TInt -> failwith if_guard_err
  | TBool -> (
    let (te2, te3) = (typeof env e2, typeof env e3) in
    if te2=te3 then te2 else failwith if_branch_err
  )
  

(**[typecheck e] checks if [e] follows the correct semantics.*)
let typecheck e =
  match typeof StaticEnvironment.empty e with
  | TInt | TBool -> true

(*Placeholder to infer type of an expression*)
let type_infer _ = failwith "todo"

*)