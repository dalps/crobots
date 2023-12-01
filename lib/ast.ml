type expr =
  | If of expr * expr
  | If_else of expr * expr * expr
  | Int_const of int
  | Term
