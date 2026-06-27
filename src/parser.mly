%{
    open Ast
%}

%token <string>ID
%token <int>INT
%token TRUE
%token FALSE
%token LEQ
%token TIMES
%token PLUS
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token EOF
%token FUN
%token TARROW
%token APP

%nonassoc IN
%nonassoc ELSE
%right TARROW
%left LEQ
%left PLUS
%left TIMES
%nonassoc INT TRUE FALSE ID
%left APP



%start <Ast.expr> prog
%%

prog:
 | e = expr; EOF { e }
 ;

expr:
 | i = INT {Int i}
 | x = ID {Var x}
 | TRUE {Bool true}
 | FALSE {Bool false}
 | e1 = expr; e2 = expr %prec APP {App (e1, e2)}
 | e1 = expr; LEQ; e2 = expr {Binop (Leq, e1, e2)}
 | e1 = expr; PLUS; e2 = expr {Binop (Add, e1, e2)}
 | e1 = expr; TIMES; e2 = expr {Binop (Mult, e1, e2)}
 | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr {Let (x, e1, e2)}
 | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr {If (e1, e2, e3)}
 | LPAREN; e = expr; RPAREN {e}
 | FUN; x = ID; TARROW; e2 = expr {Fun (x, e2)}
 


