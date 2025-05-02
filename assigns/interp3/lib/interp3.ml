include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | prog -> Some prog
  | exception _ -> None

let principle_type (ty : ty) (cs : constr list) : ty_scheme option =
  match cs with
  | Some -> Some
  | None -> None

let type_of (ctxt: stc_env) (e : expr) : ty_scheme option =
  match e with
  | Some (ty, cs) -> principle_type ty cs
  | None -> None

let is_well_typed (p : prog) : bool =
  match prog with
  | Some _ -> true
  | None -> false

exception AssertFail
exception DivByZero
exception CompareFunVals

let eval_expr (env : dyn_env) (e : expr) : value =
  match e with
  | Unit -> VUnit
  | Bool b -> VBool b
  | Int n -> VInt n
  | Float f -> VFloat f

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;binding}] -> Let {is_rec;name;binding;body = Var name}
    | {is_rec;name;binding} :: ls -> Let {is_rec;name;binding;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog ->
    if is_well_typed prog
    then Ok (eval prog)
    else Error TypeError
  | None -> Error ParseError
