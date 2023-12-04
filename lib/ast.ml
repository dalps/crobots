type bop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Gt
  | Lt
  | Geq
  | Leq
  | Land
  | Lor

type uop = UMinus

type postfix_op = Incr | Decr

type parameters = string list

type expr =
  | Var of string
  | Assign_exp of string * expr
  | Call_exp of string * expr list
  | Int_const of int
  | Unary_exp of uop * expr
  | Binary_exp of bop * expr * expr
  | Postfix_exp of postfix_op * string

type instruction =
  | If of expr * instruction
  | If_else of expr * instruction * instruction
  | While of expr * instruction
  | Exp_stat of expr
  | Return_stat of expr option
  | Null_stat
  | Compound_stat of instruction
  | Decl_var of string
  | Decl_var_init of string * expr
  | Decl_fun of string * parameters * instruction
  | Seq of instruction * instruction

type prog = instruction
