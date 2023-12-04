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
%token INCR "++"
%token DECR "--"
%token INT_TYPE "int"
%token RETURN "return"
%token LOGICAL_AND "&&" 
%token LOGICAL_OR "||" 
%token LESS_THAN "<" 
%token GREATER_THAN ">" 
%token LEQ_THAN "<=" 
%token GEQ_THAN ">=" 
%token EQUAL "==" 
%token NOT_EQUAL "!=" 
%token WHILE "while"
%token DO "do"
%token EOF

%nonassoc below_ELSE
%nonassoc ELSE

%start <prog> main

(* additional start symbols for testing *)
%start <expr> test_exp
%start <prog> test_stat

%%

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
| type_spec d = init_declarator_list ";" { d }

init_declarator_list:
| d = init_declarator { d }
| d = init_declarator "," dl = init_declarator_list { Seq(d, dl) }

init_declarator:
| id = declarator { Decl_var id }
| id = declarator "=" e = assignment_exp { Decl_var_init (id, e) }

type_spec:
| "int" {}

declarator:
| id = identifier { id }

stat:
| s = exp_stat { s }
| s = compound_stat { s }
| s = selective_stat { s }
| s = jump_stat { s }
| s = iteration_stat { s }

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

jump_stat:
| "return" e = exp? ";" { Return_stat e }

iteration_stat:
| "while" "(" e = exp ")" s = stat { While (e,s) }
| "do" s = stat "while" "(" e = exp ")" { Seq (s, While (e, s)) }

exp:
| e = assignment_exp { e }

assignment_exp:
| e = logical_or_exp { e }
| x = identifier "=" e = assignment_exp { Assign_exp (x, e) }

logical_or_exp:
| e = logical_and_exp { e }
| e1 = logical_or_exp "||" e2 = logical_and_exp { Binary_exp (Land, e1, e2) }

logical_and_exp:
| e = equality_exp { e }
| e1 = logical_and_exp "&&" e2 = equality_exp { Binary_exp (Land, e1, e2) }

equality_exp:
| e = relational_exp { e }
| e1 = equality_exp "==" e2 = relational_exp { Binary_exp (Eq, e1, e2) }
| e1 = equality_exp "!=" e2 = relational_exp { Binary_exp (Neq, e1, e2) }

relational_exp:
| e = additive_exp { e }
| e1 = relational_exp "<" e2 = additive_exp { Binary_exp (Lt, e1, e2) }
| e1 = relational_exp ">" e2 = additive_exp { Binary_exp (Gt, e1, e2) }
| e1 = relational_exp "<=" e2 = additive_exp { Binary_exp (Leq, e1, e2) }
| e1 = relational_exp ">=" e2 = additive_exp { Binary_exp (Geq, e1, e2) }

additive_exp:
| e = mult_exp { e }
| e1 = additive_exp "+" e2 = mult_exp { Binary_exp (Add, e1, e2) }
| e1 = additive_exp "-" e2 = mult_exp { Binary_exp (Sub, e1, e2) }

mult_exp:
| e = unary_exp { e }
| e1 = mult_exp "*" e2 = unary_exp { Binary_exp (Mul, e1, e2) }
| e1 = mult_exp "/" e2 = unary_exp { Binary_exp (Div, e1, e2) }
| e1 = mult_exp "%" e2 = unary_exp { Binary_exp (Mod, e1, e2) }

unary_exp:
| e = postfix_exp { e }
| "-" e = unary_exp { Unary_exp (UMinus, e) }
| "--" x = identifier { Assign_exp (x, Binary_exp (Add, Var x, Int_const 1) ) }
| "++" x = identifier { Assign_exp (x, Binary_exp (Add, Var x, Int_const 1) ) }

postfix_exp:
| e = primary_exp { e }
| x = identifier "++" { Postfix_exp (Incr, x) }
| x = identifier "--" { Postfix_exp (Decr, x) }

primary_exp:
| x = identifier { Var x }
| f = identifier "(" args = separated_list(",", assignment_exp) ")" { Call_exp(f, args) }
| n = INT_CONST { Int_const (int_of_string n) }
| "(" e = exp ")" { e }

identifier:
| x = ID { x }

