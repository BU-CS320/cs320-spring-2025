
type op = Add | Sub | Mul | Div

type expr =
  | Num of int
  | Bop of op * expr * expr

type prog = expr
