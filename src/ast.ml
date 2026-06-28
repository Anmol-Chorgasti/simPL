(**
  e ::= x | i | b | e1 bop e2
    | if e1 then e2 else e3
    | fun x -> e
    | e1 e2
    | let x = e1 in e2   (* new *)

bop ::= + | * | <=

x ::= <identifiers>

i ::= <integers>

b ::= true | false

v ::= fun x -> e | i | b
*)

type bop = 
  | Add
  | Mult
  | Leq


type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Binop of bop * expr * expr
  | Let of string * expr * expr 
  | If of expr * expr * expr
  | Fun of string * expr
  | App of expr * expr
  

type typ = 
  | TVar of string
  | TInt
  | TBool
  | TFun of typ * typ

