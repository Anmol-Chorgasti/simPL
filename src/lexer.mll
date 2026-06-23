{
    open Parser
}

let white = [' ''\t']+
let digit = ['0'-'9']
let int = '-'?digit+
let letter = ['a'-'z''A'-'Z']
let id = letter+

rule read =
 parse
 | white {read lexbuf}
 | "(" {LPAREN}
 | ")" {RPAREN}
 | "<=" {LEQ}
 | "*" {TIMES}
 | "+" {PLUS}
 | "=" {EQUALS}
 | "true" {TRUE}
 | "false" {FALSE}
 | "let" {LET}
 | "in" {IN}
 | "if" {IF}
 | "then" {THEN}
 | "else" {ELSE}
 | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
 | id {ID (Lexing.lexeme lexbuf) }
 | eof {EOF}
 