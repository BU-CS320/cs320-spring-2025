%{
open Utils
%}

%token IF
%token THEN
%token ELSE
%token LET
%token REC
%token EQ
%token IN
%token FUN
%token ARR
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token ADD
%token SUB
%token MUL
%token DIV
%token LT
%token LTE
%token GT
%token GTE
%token NEQ
%token AND
%token OR
%token<int> NUM
%token<string> VAR
%token EOF

%right OR
%right AND
%left LT, LTE, GT, GTE, EQ, NEQ
%left ADD, SUB
%left MUL, DIV

%start <Utils.prog> prog

%%

prog:
  | e = expr; EOF { e }

expr:
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LET; v = VAR; EQ; e1 = expr; IN; e2 = expr { Let (v, e1, e2) }
/*
  | LET; REC; x = VAR; EQ; e1 = expr; IN; e2 = expr
    {
      let inner_inner = Fun ("v", App (App (Var "x", Var "x"), Var "v")) in
      let inner = Fun ("x", App (Var "f", inner_inner)) in
      let fix = Fun ("f", App (inner, inner)) in
      Let (x, App (fix, Fun (x, e1)), e2)
    }
*/
  | LET; REC; x = VAR; EQ; e1 = expr; IN; e2 = expr { LetRec (x, e1, e2) }
  | FUN; v = VAR; ARR; e = expr { Fun (v, e) }
  | e = expr2 { e }

%inline bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | LT  { Lt }
  | LTE { Lte }
  | GT  { Gt }
  | GTE { Gte }
  | EQ  { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR  { Or }

expr2:
  | e1 = expr2; op = bop; e2 = expr2 { Bop (op, e1, e2) }
  | es = expr3+ { List.(fold_left (fun apps e -> App (apps, e)) (hd es) (tl es)) }

expr3:
  | TRUE { True }
  | FALSE { False }
  | n = NUM { Num n }
  | v = VAR { Var v }
  | LPAREN; e = expr; RPAREN { e }
