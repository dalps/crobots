type identifier = string

type binary_op =
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | EQ
  | NEQ
  | GT
  | LT
  | GEQ
  | LEQ
  | LAND
  | LOR

type unary_op = UMINUS

type postfix_op = INCR | DECR

type parameters = identifier list

type expression =
  | NIL
  | IDE of identifier
  | ASSIGN of identifier * expression
  | CALL of identifier * expression list
  | CALL_EXEC of instruction
  | CONST of int
  | UNARY_EXPR of unary_op * expression
  | BINARY_EXPR of expression * binary_op * expression
  | POSTFIX_EXPR of identifier * postfix_op

and instruction =
  | EMPTY
  | IF of expression * instruction
  | IFE of expression * instruction * instruction
  | WHILE of expression * instruction
  | WHILE_EXEC of expression * instruction * expression
  | EXPR of expression
  | RET of expression option
  | BLOCK of instruction
  | BLOCK_EXEC of instruction
  | VARDECL of identifier
  | VARDECL_INIT of identifier * expression
  | FUNDECL of identifier * parameters * instruction
  | SEQ of instruction * instruction

type program = instruction

let fun_of_uop = function
  | UMINUS -> ( ~- )

let int_of_bool = function
  | true -> 1
  | false -> 0

let bool_of_int = function
  | 0 -> false
  | _ -> true

let int_and x y = (bool_of_int x && bool_of_int y) |> int_of_bool
let int_or x y = (bool_of_int x || bool_of_int y) |> int_of_bool
let int_eq x y = x = y |> int_of_bool
let int_neq x y = x <> y |> int_of_bool
let int_gt x y = x > y |> int_of_bool
let int_lt x y = x < y |> int_of_bool
let int_geq x y = x >= y |> int_of_bool
let int_leq x y = x <= y |> int_of_bool

let fun_of_bop = function
  | ADD -> ( + )
  | SUB -> ( - )
  | MUL -> ( * )
  | DIV -> ( / )
  | MOD -> ( mod )
  | EQ -> int_eq
  | NEQ -> int_neq
  | GT -> int_gt
  | LT -> int_lt
  | GEQ -> int_geq
  | LEQ -> int_leq
  | LAND -> int_and
  | LOR -> int_or

exception VoidValue
