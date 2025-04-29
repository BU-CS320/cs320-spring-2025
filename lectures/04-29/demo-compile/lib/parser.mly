%{
open Utils
%}

%token <int> NUM
%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token DIV "/"
%token LPAREN "("
%token RPAREN ")"
%token EOF

%left PLUS MINUS
%left STAR DIV

%start <Utils.prog> prog

%%

prog:
  | e=expr EOF { e }

%inline bop:
  | "+" { Add }
  | "-" { Sub }
  | "*" { Mul }
  | "/" { Div }

expr:
  | e1=expr bop=bop e2=expr { Bop (bop, e1, e2) }
  | e=expr2 { e }

expr2:
  | n = NUM { Num n }
  | "(" e=expr ")" { e }
