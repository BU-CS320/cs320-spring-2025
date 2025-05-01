{
open Parser
}

let whitespace = [' ' '\n' '\t' '\r']+
let num = '-'? ['0'-'9']+

rule read =
  parse
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { DIV }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | whitespace { read lexbuf }
  | eof { EOF }
  | _ { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }
