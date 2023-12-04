open Crobots
open Ast
open Main
open Memory

let parse_expr = parse Parser.test_exp
let parse_stat = parse Parser.test_stat

let%test "int_const1" = "1" |> parse_expr = Int_const 1
let%test "int_const2" = "01" |> parse_expr = Int_const 1

let%test "if_else " =
  "if (1) {} else {}" |> parse_stat = If_else (Int_const 1, Null_stat, Null_stat)

let%test "if_no_else " = "if (1) {}" |> parse_stat = If (Int_const 1, Null_stat)

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

let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: l -> last l

let%test "trace1" =
  "{ if (1) { 1; 2; } else {}}" |> parse_stat |> trace |> last
  = Some (Exp_stat (Int_const 2))

let%test "decl_1" =
  let _ = "int x = 2; int y;" |> parse_program |> trace in
  find memory "x" = Int 2 && find memory "y" = Null

let%test "decl_2" =
  let _ = "int foo (x, y) { int x = 2; }" |> parse_program |> trace in
  find memory "x" = Null
  && find memory "y" = Null
  && find memory "foo"
     = Code ([ "x"; "y" ], Compound_stat (Decl_var_init ("x", Int_const 2)))

let%test "assign" =
  let _ = "int x = 2; int y = x + 42;" |> parse_program |> trace in
  find memory "x" = Int 2 && find memory "y" = Int 44

let%test "foo_1" =
  "
  int foo(x) { int y = 2; return x + y; }
  int main () { return 1 + foo(42); }"
  |> parse_program |> eval_main = Some 45

let%test "foo_2" =
  "
  int foo(x) { int y = 2; return x + y; }
  int main () { return bar(3) + foo(42); }
  int bar(n) { return n * 2; }"
  |> parse_program |> eval_main = Some 50

let%test "factorial_wrong" =
  "
  int fact(n) { 
    if (n == 0) return 1;
    else {
      return n * fact (n-1);
    }
  }
  int main () {
    return fact(4);
  }"
  |> parse_program |> eval_main = Some 24

let%test "factorial" =
  "
  int fact(n) {
    if (n != 0) return n * fact (n-1);
    else return 1;
  }
  int main () {
    return fact(4);
  }"
  |> parse_program |> eval_main = Some 24

let%test "factorial-ignore-expr-after-return" =
  "
  int fact(n) {
    int a;
    if (a = n > 0) { return n * fact (n-1); a = 2; } 
    else return 1;
  }
  int main () {
    return fact(4);
  }"
  |> parse_program |> eval_main = Some 24
  && find memory "a" = Int 0

let%test "prefix-incr" =
  "
  int main() {
    int x = 1;

    int y = ++x;
    
    return x == 2 && y == 2;
  }" |> parse_program |> eval_main = Some 1

let%test "postfix-decr" =
  "
  int main() {
    int x = 1, y = x--;
    
    return x == 0 && y == 1;
  }" |> parse_program |> eval_main = Some 1
