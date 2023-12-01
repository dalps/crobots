open Crobots.Ast
open Crobots.Main

let%test _ = "1" |> parse = Int_const 1
let%test _ = "01" |> parse = Int_const 1

let%test _ =
  "if (1) 2 else 3" |> parse = If_else (Int_const 1, Int_const 2, Int_const 3)

let%test _ =
  "if (1) if (0) 2 else 3" |> parse
  = If (Int_const 1, If_else (Int_const 0, Int_const 2, Int_const 3))

let%test _ =
  "1 + 2 * 3 / -(2 - 3)" |> parse
  = Add_exp
      ( Add,
        Int_const 1,
        Mul_exp
          ( Div,
            Mul_exp (Mul, Int_const 2, Int_const 3),
            Unary_exp (UMinus, Add_exp (Sub, Int_const 2, Int_const 3)) ) )
