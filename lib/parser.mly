%{
  open Ast
%}

%token TERM
%token <string>INT_CONST
%token IF "if"
%token ELSE "else"
%token LPAREN "("
%token RPAREN ")"
%token EOL

%start <expr> main 

%nonassoc below_ELSE
%nonassoc ELSE

%%

main:
| e = expr EOL { e }

expr:
| n = INT_CONST { Int_const (int_of_string n) }
| TERM { Term }
| "if" "(" e1 = expr ")" e2 = expr { If (e1,e2) } %prec below_ELSE
| "if" "(" e1 = expr ")" e2 = expr "else" e3 = expr { If_else (e1,e2,e3) }


