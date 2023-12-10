open Ast
open Memory
open Printf

let spr = sprintf

let string_of_bop = function
  | ADD -> "+"
  | SUB -> "-"
  | MUL -> "*"
  | DIV -> "/"
  | MOD -> "%"
  | EQ -> "=="
  | NEQ -> "!="
  | GT -> ">"
  | LT -> "<"
  | GEQ -> ">="
  | LEQ -> "<="
  | LAND -> "&&"
  | LOR -> "||"

let string_of_uop = function
  | UMINUS -> "-"

let string_of_postfix_op = function
  | INCR -> "++"
  | DECR -> "--"

let rec string_of_expr = function
  | IDE x -> x
  | ASSIGN (x, e) -> spr "%s = %s" x (string_of_expr e)
  | CALL (x, es) ->
      spr "%s(%s)" x (String.concat "," (List.map string_of_expr es))
  | CALL_EXEC s -> spr "exec: %s" (string_of_instr s)
  | CONST n -> spr "%d" n
  | UNARY_EXPR (uop, e) -> spr "%s%s" (string_of_uop uop) (string_of_expr e)
  | BINARY_EXPR (e1, bop, e2) ->
      spr "%s %s %s" (string_of_expr e1) (string_of_bop bop) (string_of_expr e2)
  | POSTFIX_EXPR (x, pop) -> spr "%s%s" x (string_of_postfix_op pop)

and string_of_instr = function
  | EMPTY -> ";"
  | IF (e, s) -> spr "if (%s) %s" (string_of_expr e) (string_of_instr s)
  | IFE (e, s1, s2) ->
      spr "if (%s) %s else %s" (string_of_expr e) (string_of_instr s1)
        (string_of_instr s2)
  | WHILE (e, s) | WHILE_EXEC (e, s) ->
      spr "while (%s) %s" (string_of_expr e) (string_of_instr s)
  | (SEQ (s, WHILE (e, s')) | SEQ (s, WHILE_EXEC (e, s'))) when s = s' ->
      spr "do %s while (%s);" (string_of_instr s) (string_of_expr e)
  | EXPR e -> spr "%s;" (string_of_expr e)
  | RET e ->
      spr "return %s;"
        (Option.fold ~none:"" ~some:(fun e -> string_of_expr e) e)
  | BLOCK s -> spr "{ %s }" (string_of_instr s)
  | VARDECL x -> spr "int %s;" x
  | VARDECL_INIT (x, e) -> spr "int %s = %s;" x (string_of_expr e)
  | FUNDECL (x, pars, s) ->
      spr "fun %s(%s) %s" x (String.concat "," pars) (string_of_instr s)
  | SEQ (s1, s2) -> spr "%s %s" (string_of_instr s1) (string_of_instr s2)

let string_of_memory mem =
  Hashtbl.fold (fun l v acc -> spr "%d/%d" l v :: acc) mem []
  |> String.concat "," |> spr "<%s>"

let string_of_memval = function
  | Null -> "?"
  | Loc l -> spr "[%d]" l
  | Fun _ -> "<fun>"

let string_of_envrmt env =
  Hashtbl.fold (fun x v acc -> spr "%s/%s" x (string_of_memval v) :: acc) env []
  |> String.concat "," |> spr "<%s>"
