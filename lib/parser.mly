%{
  open Ast
%}

%token <int> CONST "42" 
%token <string> IDE "x"
%token LPAREN "(" RPAREN ")"
%token LBRACE "{" RBRACE "}"
%token MUL "*" DIV "/" MOD "%"
%token ADD "+" MINUS "-"
%token LT "<" GT ">" LEQ "<=" GEQ ">=" 
%token EQ "==" NEQ "!=" 
%token LAND "&&" LOR "||" 
%token SEMICOLON ";" COMMA ","
%token ASSIGN "=" INCR "++" DECR "--"
%token INT_TYPE "int"
%token RETURN "return"
%token IF "if" ELSE "else"
%token WHILE "while" DO "do"
%token EOF

%nonassoc below_ELSE
%nonassoc ELSE

%left "||"
%left "&&"
%left "==" "!="
%left "<" ">" "<=" ">="
%left "+" "-"
%left "*" "/" "%"

%start <program> main

(* additional start symbols for testing *)
%start <expression> test_expr
%start <program> test_stat

%%

let main :=
| ~ = external_decl_list; EOF; <>

let test_expr :=
| ~ = expr; EOF; <>

let test_stat :=
| ~ = stat; EOF; <>

let external_decl_list :=
| external_decl
| ~ = external_decl; ~ = external_decl_list; <SEQ>

let external_decl := 
| function_definition
| decl

let function_definition :=
| type_spec; ~ = declarator; "("; ~ = separated_list(",", declarator); ")";
  "{"; ~ = decl_or_stat_list; "}"; <FUNDECL>

let decl := 
| type_spec; ~ = init_declarator_list; ";"; <>

let init_declarator_list :=
| init_declarator
| ~ = init_declarator; ","; ~ = init_declarator_list; <SEQ>

let init_declarator :=
| ~ = declarator; <VARDECL>
| ~ = declarator; "="; ~ = assignment_expr; <VARDECL_INIT>

let type_spec :=
| "int"

let declarator :=
| identifier

let stat :=
| expr_stat
| compound_stat
| selective_stat
| jump_stat
| iteration_stat

let expr_stat :=
| ~ = expr; ";"; <EXPR>
| ";"; { EMPTY }

let compound_stat :=
| "{"; ~ = decl_or_stat_list; "}"; <BLOCK>
| "{"; "}"; { EMPTY }

let decl_or_stat_list :=
| decl_or_stat
| ~ = decl_or_stat; ~ = decl_or_stat_list; <SEQ>

let decl_or_stat :=
| stat
| decl

let selective_stat :=
| "if"; "("; ~ = expr; ")"; ~ = stat; <IF> %prec below_ELSE
| "if"; "("; e = expr; ")"; s1 = stat; "else"; s2 = stat; { IFE (e, s1, s2) }

let jump_stat :=
| "return"; ~ = expr?; ";"; <RET>

let iteration_stat :=
| "while"; "("; e = expr; ")"; s = stat; { WHILE (e, s, e) }
| "do"; s = stat; "while"; "("; e = expr; ")"; ";"; { SEQ (s, WHILE (e, s, e)) }

let expr :=
| assignment_expr

let assignment_expr :=
| binary_expr
| ~ = identifier; "="; ~ = assignment_expr; <ASSIGN>

let binary_expr :=
| unary_expr
| e1 = binary_expr; op = binary_op; e2 = binary_expr; { BINARY_EXPR (e1, op, e2) }

let unary_expr :=
| postfix_expr
| ~ = unary_op; ~ =unary_expr; <UNARY_EXPR>
| "--"; x = identifier; { ASSIGN (x, BINARY_EXPR (IDE x, SUB, CONST 1)) }
| "++"; x = identifier; { ASSIGN (x, BINARY_EXPR (IDE x, ADD, CONST 1)) }

let postfix_expr :=
| primary_expr
| ~ = identifier; ~ = postfix_op; <POSTFIX_EXPR>

let primary_expr :=
| ~ = identifier; <IDE>
| ~ = identifier; "("; ~ = separated_list(",", assignment_expr); ")"; <CALL>
| ~ = "42"; <CONST>
| "("; ~ = expr; ")"; <>

let identifier :=
| "x"

let binary_op ==
| "*"; { MUL }
| "/"; { DIV }
| "%"; { MOD }
| "+"; { ADD }
| "-"; { SUB }
| "<"; { LT }
| ">"; { GT }
| "<="; { LEQ }
| ">="; { GEQ }
| "=="; { EQ }
| "!="; { NEQ }
| "&&"; { LAND }
| "||"; { LOR }

let unary_op ==
| "-"; { UMINUS }

let postfix_op ==
| "++"; { INCR }
| "--"; { DECR }
