{
open Parser
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "rec" { REC }
  | "=" { EQ }
  | "in" { IN }
  | "fun" { FUN }
  | "->" { ARR }
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "<>" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }
