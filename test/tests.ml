open Crobots.Ast
open Crobots.Main

let%test _ = "1" |> parse = Int_const 1
let%test _ = "01" |> parse = Int_const 1

let%test _ =
  "if (1) 2 else 3" |> parse = If_else (Int_const 1, Int_const 2, Int_const 3)

let%test _ =
  "if (1) if (0) 2 else 3" |> parse
  = If (Int_const 1, If_else (Int_const 0, Int_const 2, Int_const 3))
