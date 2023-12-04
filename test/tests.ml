open Crobots
open Ast
open Main

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
  Hashtbl.find mem "x" = Int 2 && Hashtbl.find mem "y" = Null

let%test "decl_2" =
  let _ = "int foo (x, y) { int x = 2; }" |> parse |> trace in
  Hashtbl.find mem "x" = Null
  && Hashtbl.find mem "y" = Null
  && Hashtbl.find mem "foo"
     = Code ([ "x"; "y" ], Compound_stat (Decl_var_init ("x", Int_const 2)))

let%test "assign" =
  let _ = "int x = 2; int y = x + 42;" |> parse |> trace in
  Hashtbl.find mem "x" = Int 2 && Hashtbl.find mem "y" = Int 44

let%test "foo" =
  "int foo(x) { int y = 2; return x + y; }\n\
   int main () { return 1 + foo(42); } " |> parse |> eval_main = Some 45
