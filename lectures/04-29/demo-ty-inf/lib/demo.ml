include Utils

type constr = ty * ty
type solution = (string * ty) list

let ty_subst (ty : ty) (x : string) : ty -> ty =
  let rec go = function
    | TInt -> TInt
    | TBool -> TBool
    | TFun (t1, t2) -> TFun (go t1, go t2)
    | TVar y -> if x = y then ty else TVar y
  in go

let rec free = function
  | TInt -> VarSet.empty
  | TBool -> VarSet.empty
  | TFun (t1, t2) -> VarSet.union (free t1) (free t2)
  | TVar x -> VarSet.singleton x

let rec unify (s : solution) (cs : constr list) : solution option =
  let rec go = function
    | [] -> Some (List.rev s)
    | (t1, t2) :: cs when t1 = t2 -> go cs
    | (TFun (s1, t1), TFun (s2, t2)) :: cs -> go ((s1, s2) :: (t1, t2) :: cs)
    | (TVar x, ty) :: cs when not (VarSet.mem x (free ty)) ->
      unify
        ((x, ty) :: s)
        (List.map
          (fun (t1, t2) -> (ty_subst ty x t1, ty_subst ty x t2))
          cs)
    | (ty, TVar x) :: cs -> go ((TVar x, ty) :: cs)
    | _ -> None
  in go cs
let unify = unify []

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

type stc_env = ty_scheme Env.t
let env_add x ty env = Env.add x (Forall (VarSet.empty, ty)) env

let count = ref 0

let gensym () =
  let _ = count := 1 + !count in
  TVar ("$" ^ string_of_int !count)

let instantiate (Forall (_bvs, _ty) : ty_scheme) : ty =
  assert false

let rec type_of (ctxt : stc_env) (e : expr) : ty * constr list =
  let rec go = function
    | Num _ -> TInt, []
    | Add (e1, e2) ->
      let t1, cs1 = go e1 in
      let t2, cs2 = go e2 in
      TInt, (t1, TInt) :: (t2, TInt) :: cs1 @ cs2
    | Eq (e1, e2) ->
      let t1, cs1 = go e1 in
      let t2, cs2 = go e2 in
      TBool, (t1, t2) :: cs1 @ cs2
    | If (e1, e2, e3) ->
      let t1, cs1 = go e1 in
      let t2, cs2 = go e2 in
      let t3, cs3 = go e3 in
      t3, (t1, TBool) :: (t2, t3) :: cs1 @ cs2 @ cs3
    | Let (x, e1, e2) ->
      let t1, cs1 = go e1 in
      let t2, cs2 = type_of (env_add x t1 ctxt) e2 in
      t2, cs1 @ cs2
    | Fun (x, e) ->
      let a = gensym () in
      let ty, cs = type_of (env_add x a ctxt) e in
      TFun (a, ty), cs
    | App (e1, e2) ->
      let t1, cs1 = go e1 in
      let t2, cs2 = go e2 in
      let a = gensym () in
      a, (t1, TFun (t2, a)) :: cs1 @ cs2
    | Var x ->
      match Env.find_opt x ctxt with
      | None -> TInt, [TInt, TBool]
      | Some ty_scheme -> instantiate ty_scheme, []
  in go e

let apply_solution (_s : solution) (_ty : ty) : ty =
  assert false (* Hint: fold *)

let principle_type (ty, cs) =
  match unify cs with
  | Some s ->
    let ty = apply_solution s ty in
    Some (Forall (free ty, ty))
  | None -> None

let rec eval (env : env) (e : expr) : value option =
  let rec go e =
    match e with
    | Let(x, e1, e2) -> (
      match go e1 with
        (* < env, e1 > ==> v1 *)
      | Some v1 ->
        (* < env[x -> v1] , e2 > ==> v2 *)
        eval (Env.add x v1 env) e2
      | _ -> None
    )
    | App (e1, e2) -> (
      match go e1 with
        (* < env, e1 > ==> (fun x -> e, env') *)
      | Some (VClos (x, e, env', None)) -> (
        match go e2 with
          (* < env, e2 > ==> v2 *)
        | Some v2 ->
          (* < env'[x -> v2], e > ==> v *)
          eval (Env.add x v2 env') e
        | _ -> None
      )
      | Some (VClos (x, e, env', Some f)) -> (
        match go e2 with
        | Some v2 ->
          let env' = Env.add x v2 env' in
          let env' = Env.add f (VClos (x, e, env', Some f)) env' in
          eval env' e
        | _ -> None
      )
      | _ -> None
    )
    | Var x -> Env.find_opt x env
    | Num n -> Some (VNum n)
    | Fun (x, e) -> Some (VClos (x, e, env, None))
    | Add (e1, e2) -> (
      match go e1 with
      | Some (VNum m) -> (
        match go e2 with
        | Some (VNum n) -> Some (VNum (m + n))
        | _ -> None
      )
      | _ -> None
    )
    | Eq (e1, e2) -> (
      match go e1 with
      | Some (VNum m) -> (
        match go e2 with
        | Some (VNum n) -> Some (VBool (m = n))
        | _ -> None
      )
      | _ -> None
    )
    | If (e1, e2, e3) -> (
      match go e1 with
      | Some (VBool b) ->
         if b
         then go e2
         else go e3
      | _ -> None
    )
  in go e

let rec desugar = function
  | [] -> assert false
  | (name, binding) :: [] -> Let (name, binding, Var name)
  | (name, binding) :: p ->
    Let (name, binding, desugar p)

let interp (s : string) : value option =
  match parse s with
  | Some p -> eval Env.empty (desugar p)
  | None -> None
