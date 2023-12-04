open Crobots
open Ast
open Main
open Memory

let parse_expr line : expr =
  let linebuf = Lexing.from_string line in
  Parser.test_exp Lexer.read_token linebuf

let parse_stat line : instruction =
  let linebuf = Lexing.from_string line in
  Parser.test_stat Lexer.read_token linebuf

let%test "int_const1" = "1" |> parse_expr = Int_const 1
let%test "int_const2" = "01" |> parse_expr = Int_const 1

let%test "if_no_else " =
  "if (1) {} else {}" |> parse_stat = If_else (Int_const 1, Null_stat, Null_stat)

let%test "dangling_else" =
  "if (1) if (0) {} else {}" |> parse_stat
  = If (Int_const 1, If_else (Int_const 0, Null_stat, Null_stat))

let%test "arithexpr" =
  "1 + 2 * 3 / -(2 - 3)" |> parse_expr
  = Binary_exp
      ( Add,
        Int_const 1,
        Binary_exp
          ( Div,
            Binary_exp (Mul, Int_const 2, Int_const 3),
            Unary_exp (UMinus, Binary_exp (Sub, Int_const 2, Int_const 3)) ) )

let%test "stat_list" =
  "{ 1; 2; }" |> parse_stat
  = Compound_stat (Seq (Exp_stat (Int_const 1), Exp_stat (Int_const 2)))

let rec last = function [] -> None | [ x ] -> Some x | _ :: l -> last l

let%test "trace1" =
  "{ if (1) { 1; 2; } else {}}" |> parse_stat |> trace |> last
  = Some (Exp_stat (Int_const 2))

let%test "decl_1" =
  let _ = "int x = 2; int y;" |> parse |> trace in
  find memory "x" = Int 2 && find memory "y" = Null

let%test "decl_2" =
  let _ = "int foo (x, y) { int x = 2; }" |> parse |> trace in
  find memory "x" = Null
  && find memory "y" = Null
  && find memory "foo"
     = Code ([ "x"; "y" ], Compound_stat (Decl_var_init ("x", Int_const 2)))

let%test "assign" =
  let _ = "int x = 2; int y = x + 42;" |> parse |> trace in
  find memory "x" = Int 2 && find memory "y" = Int 44

let%test "foo_1" =
  "int foo(x) { int y = 2; return x + y; }\n\
  \   int main () { return 1 + foo(42); } " |> parse |> eval_main = Some 45

let%test "foo_2" =
  "\n\
  \  int foo(x) { int y = 2; return x + y; }\n\
  \  int main () { return bar(3) + foo(42); }\n\
  \  int bar(n) { return n * 2; }\n" |> parse |> eval_main = Some 50

let%test "factorial_wrong" =
  "\n\
  \  int fact(n) { \n\
  \    if (n) return 1;\n\
  \    else {\n\
  \      return n * fact (n-1);\n\
  \    }\n\
  \  }\n\
  \  int main () {\n\
  \    return fact(4);\n\
  \  }\n" |> parse |> eval_main = Some 1

let%test "factorial" =
  "\n\
  \  int fact(n) {\n\
  \    if (n) return n * fact (n-1);\n\
  \    else return 1;\n\
  \  }\n\
  \  int main () {\n\
  \    return fact(4);\n\
  \  }\n" |> parse |> eval_main = Some 24
