%{
  open Ast
%}

%token <string>INT_CONST
%token IF "if"
%token ELSE "else"
%token LPAREN "("
%token RPAREN ")"
%token MUL "*"
%token DIV "/"
%token MOD "%"
%token MINUS "-"
%token ADD "+"
%token EOL

%start <expr> main 

%nonassoc below_ELSE
%nonassoc ELSE

%%

main:
| e = exp EOL { e }

exp:
| e = selective_exp { e }

selective_exp:
| e = additive_exp { e }
| "if" "(" e1 = exp ")" e2 = exp { If (e1,e2) } %prec below_ELSE
| "if" "(" e1 = exp ")" e2 = exp "else" e3 = exp { If_else (e1,e2,e3) }

additive_exp:
| e = mult_exp { e }
| e1 = additive_exp "+" e2 = mult_exp { Add_exp (Add, e1, e2) }
| e1 = additive_exp "-" e2 = mult_exp { Add_exp (Sub, e1, e2) }

mult_exp:
| e = unary_exp { e }
| e1 = mult_exp "*" e2 = unary_exp { Mul_exp (Mul, e1, e2)}
| e1 = mult_exp "/" e2 = unary_exp { Mul_exp (Div, e1, e2)}
| e1 = mult_exp "%" e2 = unary_exp { Mul_exp (Mod, e1, e2)}

unary_exp:
| e = primary_exp { e }
| "-" e = unary_exp { Unary_exp ((UMinus), e)}

primary_exp:
| n = INT_CONST { Int_const (int_of_string n) }
| "(" e = exp ")" { e }

