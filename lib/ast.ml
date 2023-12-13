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

let remove_block = function
  | BLOCK s | s -> s
