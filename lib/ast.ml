type bop = Add | Sub | Mul | Div | Mod
type uop = UMinus

type expr =
  | If of expr * expr
  | If_else of expr * expr * expr
  | Int_const of int
  | Term
  | Unary_exp of uop * expr
  | Add_exp of bop * expr * expr
  | Mul_exp of bop * expr * expr
