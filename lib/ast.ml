type bop = Add | Sub | Mul | Div | Mod
type uop = UMinus

type expr =
  | Int_const of int
  | Unary_exp of uop * expr
  | Add_exp of bop * expr * expr
  | Mul_exp of bop * expr * expr

type stat =
  | If of expr * stat
  | If_else of expr * stat * stat
  | Exp_stat of expr
  | Null_stat
  | Compound_stat of stat
  | Stat_list of stat * stat

type prog = stat
