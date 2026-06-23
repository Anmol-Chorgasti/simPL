open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let e = Parser.prog Lexer.read lexbuf in
  e

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
  | Var _ -> failwith "unbound variable"
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
  | If (Int _, _, _) -> failwith "Guard of if must be Bool value"
  | If (e1, e2, e3) -> step (If (step e1, e2, e3))
and 
step_bop bop e1 e2 = 
  match bop, e1, e2 with
  | Add, Int x, Int y -> Int (x + y)
  | Mult, Int x, Int y -> Int (x * y)
  | Leq, Int x, Int y -> Bool (x <= y)
  | _ -> failwith "invalid combination of operator and operands"

(**[eval e] evaluates [e] completely to some value [v].*)
let rec eval e =
  if is_value e then e else eval (step e)

(**[interp s] evaluates [s] to [v] and returns [v] in string format. *)
let interp s = 
  s |> parse |> eval |> string_of_val