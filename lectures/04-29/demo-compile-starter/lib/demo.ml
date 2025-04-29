include Utils

let parse s = Parser.prog Lexer.read (Lexing.from_string s)

let emit = output_byte stdout

let emit_int n = emit 4; output_binary_int stdout n
let emit_op op =
  let code_of_op = function
    | Add -> 0
    | Sub -> 1
    | Mul -> 2
    | Div -> 3
  in emit (code_of_op op)

let compile = assert false

let compile_str s =
  let rec go = function
    | Num n -> ["PUSH " ^ string_of_int n]
    | Bop (op, e1, e2) ->
      let op_str =
        match op with
        | Add -> "ADD"
        | Sub -> "SUB"
        | Mul -> "MUL"
        | Div -> "DIV"
      in go e2 @ go e1 @ [op_str]
  in String.concat "\n" (go s)
