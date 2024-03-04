open Ast
open Memory
open Robot
open Printf
open Math

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
  | LSHIFT -> "<<"
  | RSHIFT -> ">>"

let string_of_uop = function
  | UMINUS -> "-"
  | LNOT -> "!"

let rec string_of_expr = function
  | NIL -> "nil"
  | IDE x -> x
  | ASSIGN (x, e) -> spr "%s = %s" x (string_of_expr e)
  | CALL (x, es) ->
      spr "%s(%s)" x (String.concat "," (List.map string_of_expr es))
  | CALL_EXEC s -> spr "<exec: %s>" (string_of_instr s)
  | CONST n -> spr "%d" n
  | UNARY_EXPR (uop, e) -> spr "%s%s" (string_of_uop uop) (string_of_expr e)
  | BINARY_EXPR (e1, bop, e2) ->
      spr "%s %s %s" (string_of_expr e1) (string_of_bop bop) (string_of_expr e2)

and string_of_instr = function
  | EMPTY -> ";"
  | IF (e, s) -> spr "if (%s) %s" (string_of_expr e) (string_of_instr s)
  | IFE (e, s1, s2) ->
      spr "if (%s) %s else %s" (string_of_expr e) (string_of_instr s1)
        (string_of_instr s2)
  | WHILE (e, s) | WHILE_EXEC (e, s) ->
      spr "while (%s) %s" (string_of_expr e) (string_of_instr s)
  | SEQ (s, WHILE_EXEC (e, _)) ->
      spr "do %s while (%s);" (string_of_instr s) (string_of_expr e)
  | EXPR e -> spr "%s;" (string_of_expr e)
  | RET e ->
      spr "return %s;"
        (Option.fold ~none:"" ~some:(fun e -> string_of_expr e) e)
  | BLOCK s | BLOCK_EXEC s -> spr "{ %s }" (string_of_instr s)
  | VARDECL x -> spr "int %s;" x
  | VARDECL_INIT (x, e) -> spr "int %s = %s;" x (string_of_expr e)
  | FUNDECL (x, pars, s) ->
      spr "fun %s(%s) { %s }" x (String.concat "," pars) (string_of_instr s)
  | SEQ (s1, s2) -> spr "%s %s" (string_of_instr s1) (string_of_instr s2)

let string_of_memory mem =
  Hashtbl.fold (fun l v acc -> spr "%d/%d" l v :: acc) mem []
  |> String.concat ", " |> spr "[%s]"

let string_of_intrinsic = function
  | SCAN -> "scan"
  | CANNON -> "canon"
  | DRIVE -> "drive"
  | DAMAGE -> "damage"
  | SPEED -> "speed"
  | HEADING -> "heading"
  | LOC_X -> "loc_x"
  | LOC_Y -> "loc_y"
  | RAND -> "rand"
  | SQRT -> "sqrt"
  | SIN -> "sin"
  | COS -> "cos"
  | TAN -> "tan"
  | ATAN -> "atan"

let string_of_memval = function
  | Loc l -> spr "%d" l
  | Fun _ | Intrinsic _ -> "<fun>"

let rec remove_duplicates s =
  match Seq.uncons s with
  | None -> Seq.empty
  | Some ((x, v), s') ->
      let s' = Seq.drop_while (fun (y, _) -> y = x) s' in
      Seq.cons (x, v) (remove_duplicates s')

let string_of_envrmt env =
  let s = Hashtbl.to_seq env |> remove_duplicates in
  Seq.fold_left
    (fun acc (x, v) ->
      match v with
      | Intrinsic _ -> acc
      | _ -> spr "%s/%s" x (string_of_memval v) :: acc)
    [] s
  |> String.concat ", " |> spr "{%s}"

let string_of_trace es =
  List.map (fun e -> spr "%s" (string_of_expr e)) es |> String.concat "\n"

let string_of_trace_st es =
  List.map
    (fun (env, mem, e) ->
      spr "%s %s %s" (string_of_expr e) (string_of_envrmt env)
        (string_of_memory mem))
    es
  |> String.concat "\n"

let string_of_robot (r : Robot.t) =
  spr
    "x: %3.2f, y: %3.2f, sp: %3.2f, dsp: %3.2f, hd: %3.2f, dmg: %3.2f, sc: %3.2f, rel: %3d"
    r.p.x r.p.y r.speed r.d_speed r.heading r.damage r.scan_degrees r.reload

let string_of_all_robots (rs : Robot.t array) =
  Array.to_seq rs
  |> Seq.fold_lefti
       (fun acc i r ->
         match r.status with
         | ALIVE -> acc ^ spr "(%d. %12s) %s\n" i r.name (string_of_robot r)
         | DEAD -> acc ^ spr "(%d. %12s) KO\n" i r.name)
       ""
