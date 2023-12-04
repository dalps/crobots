type bop = Add | Sub | Mul | Div | Mod
type uop = UMinus

type parameters = string list

type expr =
  | Var of string
  | Assign_exp of string * expr
  | Call_exp of string * expr list
  | Int_const of int
  | Unary_exp of uop * expr
  | Binary_exp of bop * expr * expr

type instruction =
  | If of expr * instruction
  | If_else of expr * instruction * instruction
  | Exp_stat of expr
  | Return_stat of expr option
  | Null_stat
  | Compound_stat of instruction
  | Decl_var of string
  | Decl_var_init of string * expr
  | Decl_fun of string * parameters * instruction
  | Seq of instruction * instruction

type prog = instruction (* refactor to `instruction list` and remove `Seq` *)
