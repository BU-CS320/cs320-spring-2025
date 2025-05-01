
type command =
  | PUSH | ADD | SUB | MUL | DIV | LT
  | IF
  | FUN | CALL | RETURN | FUNREC
  | LOOKUP

let command_of_byte c =
  let go = function
    | 0 -> PUSH
    | 1 -> ADD
    | 2 -> SUB
    | 3 -> MUL
    | 4 -> DIV
    | 5 -> LT
    | 6 -> IF
    | 7 -> FUN
    | 8 -> CALL
    | 9 -> RETURN
    | 10 -> LOOKUP
    | 11 -> FUNREC
    | _ -> failwith "unknown code"
  in go (int_of_char c)

type value =
  | Num of int32
  | Clos of int * value list
  | ClosRec of int * value list

let string_of_value = function
  | Num n -> Int32.to_string n
  | _ -> "<fun>"

let prog = Bytes.of_string In_channel.(input_all stdin)
let get_int32 bs = Bytes.get_int32_be prog bs
let get_int bs = Int32.to_int (get_int32 bs)

let rec eval (stack, env, pc) =
  if pc = Bytes.length prog
  then
    match stack with
    | [x] -> x
    | _ -> failwith "invalid final stack"
  else eval (eval_step stack env pc)
and eval_step stack env pc =
  match command_of_byte (Bytes.get prog pc), stack with
  | PUSH, stack ->
    Num (get_int32 (pc + 1)) :: stack, env, pc + 5
  | ADD, Num m :: Num n :: stack ->
    Num (Int32.add m n) :: stack, env, pc + 1
  | SUB, Num m :: Num n :: stack ->
    Num (Int32.sub m n) :: stack, env, pc + 1
  | MUL, Num m :: Num n :: stack ->
    Num (Int32.mul m n) :: stack, env, pc + 1
  | DIV, Num m :: Num n :: stack ->
    Num (Int32.div m n) :: stack, env, pc + 1
  | LT, Num m :: Num n :: stack ->
    Num (if Int32.compare m n < 0 then 1l else 0l) :: stack, env, pc + 1
  | IF, (Num 0l) :: stack ->
    stack, env, pc + 5 + get_int (pc + 1)
  | IF, Num _ :: stack ->
    stack, env, pc + 5
  | FUN, stack ->
    Clos (pc + 5, env) :: stack, env, pc + 5 + get_int (pc + 1)
  | FUNREC, stack ->
    ClosRec (pc + 5, env) :: stack, env, pc + 5 + get_int (pc + 1)
  | CALL, Clos (fun_pc, fun_env) :: v :: stack ->
    Clos (pc + 1, env) :: stack, v :: fun_env, fun_pc
  | CALL, ClosRec (fun_pc, fun_env) :: v :: stack ->
    Clos (pc + 1, env) :: stack, ClosRec (fun_pc, fun_env) :: v :: fun_env, fun_pc
  | RETURN, v :: Clos (ret_pc, ret_env) :: stack ->
    v :: stack, ret_env, ret_pc
  | LOOKUP, stack ->
    List.nth env (get_int (pc + 1)) :: stack, env, pc + 5
  | _ -> failwith "whoops"

let () =
  ([], [], 0)
  |> eval
  |> string_of_value
  |> print_endline
