type bop =
  | Add | Sub | Mul | Div
  | Lt | Lte | Gt | Gte | Eq | Neq
  | And | Or

type expr =
  | True | False
  | Num of int
  | Var of string
  | App of expr * expr
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | LetRec of string * expr * expr
  | Fun of string * expr

type prog = expr
