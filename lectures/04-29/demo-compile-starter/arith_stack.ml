type command = ADD | SUB | MUL | DIV | PUSH of int

let command_of_byte = function
  | 0 -> ADD
  | 1 -> SUB
  | 2 -> MUL
  | 3 -> DIV
  | 4 -> PUSH (input_binary_int stdin)
  | _ -> failwith "unknown code"

let eval () =
  let rec go s =
    match s, command_of_byte (input_byte stdin) with
    | m :: n :: s, ADD -> go ((m + n) :: s)
    | _, ADD -> failwith "stack underflow: ADD"
    | m :: n :: s, SUB -> go ((m - n) :: s)
    | _, SUB -> failwith "stack underflow: SUB"
    | m :: n :: s, MUL -> go ((m * n) :: s)
    | _, MUL -> failwith "stack underflow: MUL"
    | m :: 0 :: s, DIV -> failwith "division by zero"
    | m :: n :: s, DIV -> go ((m / n) :: s)
    | _, DIV -> failwith "stack underflow: DIV"
    | s, PUSH n -> go (n :: s)
    | exception End_of_file -> s
  in
    match go [] with
    | [x] -> x
    | _ -> failwith "error: invalid computation"

let () = print_endline (string_of_int (eval ()))
