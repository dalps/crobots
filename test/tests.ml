open Crobots.Ast
open Crobots.Main

let%test "int_const1" = "1;" |> parse = Exp_stat (Int_const 1)
let%test "int_const2" = "01;" |> parse = Exp_stat (Int_const 1)

let%test "if_no_else " =
  "if (1) {} else {}" |> parse = If_else (Int_const 1, Null_stat, Null_stat)

let%test "dangling_else" =
  "if (1) if (0) {} else {}" |> parse
  = If (Int_const 1, If_else (Int_const 0, Null_stat, Null_stat))

let%test "arithexpr" =
  "1 + 2 * 3 / -(2 - 3);" |> parse
  = Exp_stat
      (Add_exp
         ( Add,
           Int_const 1,
           Mul_exp
             ( Div,
               Mul_exp (Mul, Int_const 2, Int_const 3),
               Unary_exp (UMinus, Add_exp (Sub, Int_const 2, Int_const 3)) ) ))

let%test "stat_list" =
  "{ 1; 2; }" |> parse
  = Compound_stat (Stat_list (Exp_stat (Int_const 1), Exp_stat (Int_const 2)))

let rec last = function [] -> None | [ x ] -> Some x | _ :: l -> last l

let%test "trace1_1" =
  "{ if (1) { 1; 2; } else {}}" |> parse |> trace |> last
  = Some (Exp_stat (Int_const 2))
