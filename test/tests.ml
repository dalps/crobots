open Crobots
open Ast
open Main
open Eval
open Trace

let parse_expr = parse' Parser.test_expr
let parse_stat = parse' Parser.test_stat

let%test "int_const1" = "1" |> parse_expr = CONST 1
let%test "int_const2" = "01" |> parse_expr = CONST 1

let%test "if_else " =
  "if (1) {} else {}" |> parse_stat = IFE (CONST 1, EMPTY, EMPTY)

let%test "if_no_else " = "if (1) {}" |> parse_stat = IF (CONST 1, EMPTY)

let%test "dangling_else" =
  "if (1) if (0) {} else {}" |> parse_stat
  = IF (CONST 1, IFE (CONST 0, EMPTY, EMPTY))

let%test "arithexpr" =
  "1 + 2 * 3 / -(2 - 3)" |> parse_expr
  = BINARY_EXPR
      ( CONST 1,
        ADD,
        BINARY_EXPR
          ( BINARY_EXPR (CONST 2, MUL, CONST 3),
            DIV,
            UNARY_EXPR (UMINUS, BINARY_EXPR (CONST 2, SUB, CONST 3)) ) )

let%test "stat_list" =
  "{ 1; 2; }" |> parse_stat = BLOCK (SEQ (EXPR (CONST 1), EXPR (CONST 2)))

let rec last = function
  | [] -> failwith "last on empty list"
  | [ x ] -> x
  | _ :: l -> last l

let%test "foo_1" =
  "
  int foo(x) { int y = 2; return x + y; }
  int main () { return 1 + foo(42); }"
  |> parse |> eval = Some 45

let%test "foo_2" =
  "
  int foo(x) { int y = 2; return x + y; }
  int main () { return bar(3) + foo(42); }
  int bar(n) { return n * 2; }"
  |> parse |> eval = Some 50

let%test "foo_3" =
  "
  int foo(x) { int y = 2; return x + y; }
  int main () { return foo(z); }
  int z = 2;
  "
  |> parse |> eval = Some 4

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
  |> parse |> eval = Some 24

let%test "factorial" =
  "
  int fact(n) {
    if (n != 0) return n * fact (n-1);
    else return 1;
  }
  int main () {
    return fact(4);
  }"
  |> parse |> eval = Some 24

let%test "factorial-ignore-expr-after-return" =
  "
  int fact(n) {
    int a;
    if (a = n > 0) { return n * fact (n-1); a = 2; } 
    else return 1;
  }
  int main () {
    return fact(6);
  }"
  |> parse |> eval = Some 720

let%test "prefix-incr" =
  "
  int main() {
    int x = 1;

    int y = ++x;
    
    return x == 2 && y == 2;
  }"
  |> parse |> eval = Some 1

let%test "postfix-decr" =
  "
  int main() {
    int x = 1, y = x--;
    
    return x == 0 && y == 1;
  }"
  |> parse |> eval = Some 1

let%test "factorial-iterative" =
  "
  int fact(n) {
    int acc = 1;

    while (n) {
      acc = acc * n; 
      n = n - 1;
    }
    
    return acc;
  }

  int main () {
    return fact(4);
  }"
  |> parse |> eval = Some 24

let%test "do-while" =
  "
  int main () {
    int i = 1;
    int x = 2;
    do {
      x = x * 2;
    }
    while (i != 1);

    return x;
  }"
  |> parse |> eval = Some 4

let%test "do-while-shadow" =
  "
  int main () {
    int i = 1;
    int x = 2;

    do {
      int x = 42;
      x++;
    }
    while (i != 1);

    return x;
  }"
  |> parse |> eval = Some 2

let%test "block" =
  "
  int main() {
    int y;
    int x;
    x = 50;
    {
      int x;
      x = 40;
      y = x+2;
    }

    return x == 50 && y == 42;
  }"
  |> parse |> eval = Some 1

let%test "many-args" =
  "
  int foo(w,x,y,z) { return w * x + y * z; }
  int main() {
    int a = 2;
    return foo(10,a,3/a,42);
  }"
  |> parse |> eval = Some 62

let%test "many-args2" =
  "
  int foo(w,x,y,z) { return x * y + w * z; }
  int main() {
    int a = 2;
    return foo(10,a,3/a,42);
  }"
  |> parse |> eval = Some 422

let%test "many-args3" =
  "
  int foo(w,x,y,z) { return x * y + w * z; }
  int main() {
    int a = 2;
    return foo(8 + a,a,3/a,42 + a * 0);
  }"
  |> parse |> eval = Some 422

let%test "side-effect" =
  "
  int x;
  int foo() { --x; }
  int main() {
    x = 42;
    foo();
    return x;
  }"
  |> parse |> eval = Some 41

let%test "side-effect-trace" =
  "
  int x;
  int foo() { --x; }
  int main() {
    x = 42;
    foo();
    return x;
  }"
  |> parse |> trace |> last = CONST 41

let%test "many-args-trace" =
  "
  int foo(w,x,y,z) { return x * y + w * z; }
  int main() {
    int a = 2;
    return foo(8 + a,a,3/a,42 + a * 0);
  }"
  |> parse |> trace |> last = CONST 422
