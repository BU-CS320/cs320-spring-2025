include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

let desugar (prog : prog) : expr =
  let rec desugar_of_prog prog acc =
    match prog with
    | [] -> acc
    | (rec_flag, name, args, ty, body) :: rest ->
      let curry_ed = List.fold_right (fun (_, t) acc -> FunTy (t, acc)) args ty

let type_of (e : expr) : (ty, error) result =
  match e with
  | Num _ -> Ok IntTy
  | Bool _ -> Ok BoolTy
  | Unit -> Ok UnitTy

exception AssertFail
exception DivByZero

let eval (e : expr) :  value =
  match e with
  | Num n -> VNum n
  | Bool b -> VBool b
  | Unit -> VUnit

let interp (s : string) : (value, error) result =
  match parse s with
  | None -> Error ParseFail
    let e = desugar prog in
    match type_of e with
    | Ok _ -> Ok
    | Error err -> Error err
