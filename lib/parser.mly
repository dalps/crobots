%{
  open Ast
%}

%token <string> INT_CONST 
%token <string> ID
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
%token COMMA ","
%token ASSIGN "="
%token INT_TYPE "int"
%token EOF

%nonassoc below_ELSE
%nonassoc ELSE

%start <prog> main

(* additional start symbols for testing *)
%start <expr> test_exp
%start <prog> test_stat

%%

(* there's a clear distinction between declarations and statements: statements are part of declarations *)

main:
| p = external_decl_list EOF { p }

test_exp:
| e = exp EOF { e }

test_stat:
| s = stat EOF { s }

external_decl_list:
| d = external_decl { d }
| d = external_decl dl = external_decl_list { Seq (d, dl) }

external_decl: 
| d = function_definition  { d }
| d = decl { d }

function_definition:
| type_spec id = declarator "(" pars = separated_list(",", declarator) ")" s = compound_stat { Decl_fun (id, pars, s) }

decl: 
| type_spec d = init_declarator ";" { d }

init_declarator:
| id = declarator { Decl_var id }
| id = declarator "=" e = exp { Decl_var_init (id, e) }

type_spec:
| "int" {}

declarator:
| id = ID { id }

stat:
| s = exp_stat { s }
| s = compound_stat { s }
| s = selective_stat { s }

exp_stat:
| e = exp ";" { Exp_stat e } 
| ";" { Null_stat }

compound_stat:
| "{" xl = decl_or_stat_list "}" { Compound_stat xl }
| "{" "}" { Null_stat }

decl_or_stat_list:
| x = decl_or_stat { x }
| x = decl_or_stat xl = decl_or_stat_list { Seq (x, xl) }

decl_or_stat:
| s = stat { s }
| d = decl { d }

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

