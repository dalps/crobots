%{
  open Ast
%}

%token <string>INT_CONST 
%token IF "if"
%token ELSE "else"
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token MUL "*"
%token DIV "/"
%token MOD "%"
%token MINUS "-"
%token ADD "+"
%token SEMICOLON ";"
%token EOL

%type <expr> exp
%type <stat> stat

%start <prog> main 

%nonassoc below_ELSE
%nonassoc ELSE

%%

main:
| p = stat EOL { p }

stat:
| s = exp_stat { s }
| s = compound_stat { s }
| s = selective_stat { s }

exp_stat:
| e = exp ";" { Exp_stat e } 
| ";" { Null_stat }

compound_stat:
| "{" sl = stat_list "}" { Compound_stat sl }
| "{" "}" { Null_stat }

stat_list:
| s = stat { s }
| s = stat sl = stat_list { Stat_list (s, sl) } (* stat lists are left associative *)

selective_stat:
| "if" "(" e = exp ")" s = stat { If (e,s) } %prec below_ELSE
| "if" "(" e = exp ")" s1 = stat "else" s2 = stat { If_else (e,s1,s2) }

exp:
| e = additive_exp { e }

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

