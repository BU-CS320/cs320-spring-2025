include Utils

let parse s = Parser.prog Lexer.read (Lexing.from_string s)

type lexpr =
  | True | False
  | Num of int
  | Var of int
  | App of lexpr * lexpr
  | Bop of bop * lexpr * lexpr
  | If of lexpr * lexpr * lexpr
  | Fun of lexpr
  | FunRec of lexpr

let lexpr_of_expr =
  let rec go vars =
    let rec go' : expr -> lexpr = function
      | True -> True
      | False -> False
      | Num n -> Num n
      | Var x -> (
        match List.find_index ((=) x) vars with
        | Some n -> Var n
        | None -> failwith "unknown variable"
      )
      | App (e1, e2) -> App (go' e1, go' e2)
      | Bop (op, e1, e2) -> Bop (op, go' e1, go' e2)
      | If (e1, e2, e3) -> If (go' e1, go' e2, go' e3)
      | Let (x, e1, e2) -> App (Fun (go (x :: vars) e2), go' e1)
      | Fun (x, e) -> Fun (go (x :: vars) e)
      | LetRec (f, Fun (x, e1), e2) -> App (Fun (go (f :: vars) e2), FunRec (go (f :: x :: vars) e1))
      | LetRec (_, _, _) -> failwith "bad recursive definition"
    in go'
  in go []

let emit = output_byte stdout
let emit_int n = output_binary_int stdout n
let emit_op op =
  let code_of_op = function
    | Add -> 1
    | Sub -> 2
    | Mul -> 3
    | Div -> 4
    | Lt -> 5
    | _ -> failwith "unknown op"
  in emit (code_of_op op)
let emit_push () = emit 0
let emit_add () = emit 1
let emit_sub () = emit 2
let emit_mul () = emit 3
let emit_div () = emit 4
let emit_lt () = emit 5
let emit_if () = emit 6
let emit_fun () = emit 7
let emit_call () = emit 8
let emit_return () = emit 9
let emit_lookup () = emit 10
let emit_funrec () = emit 11

let andd e1 e2 = If (e1, e2, False)
let orr e1 e2 = If (e1, True, e2)
let nott e1 = If(e1, False, True)
let lt e1 e2 = Bop (Lt, e1, e2)
let gte e1 e2 = nott (lt e1 e2)
let eq e1 e2 = andd (gte e1 e2) (gte e2 e1)
let neq e1 e2 = nott (eq e1 e2)
let lte e1 e2 = orr (lt e1 e2) (eq e1 e2)
let gt e1 e2 = nott (lte e1 e2)

let rec offset (e : lexpr) : int =
  let int_offset = 4 in
  match e with
  | True -> 1 + int_offset
  | False -> 1 + int_offset
  | Num _ -> 1 + int_offset
  | Var _ -> 1 + int_offset
  | App (e1, e2) -> offset e1 + offset e2 + 1
  | Bop (Add, e1, e2) -> offset e1 + offset e2 + 1
  | Bop (Sub, e1, e2) -> offset e1 + offset e2 + 1
  | Bop (Mul, e1, e2) -> offset e1 + offset e2 + 1
  | Bop (Div, e1, e2) -> offset e1 + offset e2 + 1
  | Bop (Lt, e1, e2) -> offset e1 + offset e2 + 1
  | Bop (Lte, e1, e2) -> offset (lte e1 e2)
  | Bop (Gt, e1, e2) -> offset (gt e1 e2)
  | Bop (Gte, e1, e2) -> offset (gte e1 e2)
  | Bop (Eq, e1, e2) -> offset (eq e1 e2)
  | Bop (Neq, e1, e2) -> offset (neq e1 e2)
  | Bop (And, e1, e2) -> offset (andd e1 e2)
  | Bop (Or, e1, e2) -> offset (orr e1 e2)
  | If (e1, e2, e3) ->
     offset e1                          (* e1 *)
     + 1 + int_offset                   (* IF offset *)
     + offset e2                        (* e2 *)
     + 1 + int_offset + 1 + int_offset  (* PUSH 0 IF OFFSET *)
     + offset e3                        (* e3 *)
  | Fun e -> 1 + int_offset + offset e + 1
  | FunRec e -> 1 + int_offset + offset e + 1

let compile_str e =
  let rec go (e : lexpr) =
    match e with
    | True -> ["PUSH 1"]
    | False -> ["PUSH 0"]
    | Num n -> ["PUSH " ^ string_of_int n]
    | Var n -> ["LOOKUP " ^ string_of_int n]
    | App (e1, e2) -> go e2 @ go e1 @ ["CALL"]
    | Bop (Add, e1, e2) -> go e2 @ go e1 @ ["ADD"]
    | Bop (Sub, e1, e2) -> go e2 @ go e1 @ ["SUB"]
    | Bop (Mul, e1, e2) -> go e2 @ go e1 @ ["MUL"]
    | Bop (Div, e1, e2) -> go e2 @ go e1 @ ["DIV"]
    | Bop (Lt, e1, e2) -> go e2 @ go e1 @ ["LT"]
    | Bop (Lte, e1, e2) -> go (lte e1 e2)
    | Bop (Gt, e1, e2) -> go (gt e1 e2)
    | Bop (Gte, e1, e2) -> go (gte e1 e2)
    | Bop (Eq, e1, e2) -> go (eq e1 e2)
    | Bop (Neq, e1, e2) -> go (neq e1 e2)
    | Bop (And, e1, e2) -> go (andd e1 e2)
    | Bop (Or, e1, e2) -> go (orr e1 e2)
    | If (e1, e2, e3) ->
        go e1
        @ ["IF " ^ string_of_int (offset e2 + 1 + 4 + 1 + 4)]
        @ go e2
        @ [ "PUSH 0"; "IF " ^ string_of_int (offset e3) ]
        @ go e3
    | Fun e -> ("FUN " ^ string_of_int (offset e + 1)) :: go e @ ["RETURN"]
    | FunRec e -> ("FUNREC " ^ string_of_int (offset e + 1)) :: go e @ ["RETURN"]
 in String.concat "\n" (go e)

let rec compile (e : lexpr) =
  match e with
  | True -> emit_push (); emit_int 1
  | False -> emit_push (); emit_int 0
  | Num n -> emit_push (); emit_int n
  | Var n -> emit_lookup (); emit_int n
  | App (e1, e2) -> compile e2; compile e1; emit_call ()
  | Bop (Add, e1, e2) -> compile e2; compile e1; emit_add ()
  | Bop (Sub, e1, e2) -> compile e2; compile e1; emit_sub ()
  | Bop (Mul, e1, e2) -> compile e2; compile e1; emit_mul ()
  | Bop (Div, e1, e2) -> compile e2; compile e1; emit_div ()
  | Bop (Lt, e1, e2) -> compile e2; compile e1; emit_lt ()
  | Bop (Lte, e1, e2) -> compile (lte e1 e2)
  | Bop (Gt, e1, e2) -> compile (gt e1 e2)
  | Bop (Gte, e1, e2) -> compile (gte e1 e2)
  | Bop (Eq, e1, e2) -> compile (eq e1 e2)
  | Bop (Neq, e1, e2) -> compile (neq e1 e2)
  | Bop (And, e1, e2) -> compile (andd e1 e2)
  | Bop (Or, e1, e2) -> compile (orr e1 e2)
  | If (e1, e2, e3) -> (
     compile e1;
     emit_if ();
     emit_int (offset e2 + 1 + 4 + 1 + 4);
     compile e2;
     emit_push (); emit_int 0;
     emit_if ();
     emit_int (offset e3);
     compile e3;
  )
  | Fun e -> (
     emit_fun ();
     emit_int (offset e + 1);
     compile e;
     emit_return ()
  )
  | FunRec e -> (
     emit_funrec ();
     emit_int (offset e + 1);
     compile e;
     emit_return ()
  )

let compile as_str e =
  if as_str
  then e |> lexpr_of_expr |> compile_str |> print_endline
  else e |> lexpr_of_expr |> compile
