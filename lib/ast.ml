type bop = Add | Sub | Mul | Div | Mod
type uop = UMinus

type expr =
  | Var of string
  | Assign_exp of string * expr
  | Int_const of int
  | Unary_exp of uop * expr
  | Add_exp of bop * expr * expr
  | Mul_exp of bop * expr * expr

type instruction =
  | If of expr * instruction
  | If_else of expr * instruction * instruction
  | Exp_stat of expr
  | Null_stat
  | Compound_stat of instruction
  | Decl_var of string
  | Decl_var_init of string * expr
  | Decl_fun of string * string list * instruction
  | Seq of instruction * instruction

type prog = instruction
