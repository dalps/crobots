open Ast
open Memory

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

let rec eval_expr = function
  | CONST n -> Some n
  | IDE x -> Some (read_var x)
  | ASSIGN (x, e) -> (
      match eval_expr e with
      | Some n ->
          update_var x n;
          Some n
      | None -> raise VoidValue)
  | CALL (f, es) ->
      add_frame ();
      let pars, s = read_fun f in
      let o =
        List.iter2
          (fun x e ->
            match eval_expr e with
            | Some n -> add_var ~init:n x
            | None -> raise VoidValue)
          pars es;
        eval_program s
      in
      ();
      ignore (pop_frame ());
      o
  | UNARY_EXPR (uop, e) -> (
      match eval_expr e with
      | Some n -> Some ((fun_of_uop uop) n)
      | None -> raise VoidValue)
  | BINARY_EXPR (e1, bop, e2) -> (
      match (eval_expr e1, eval_expr e2) with
      | Some n1, Some n2 -> Some ((fun_of_bop bop) n1 n2)
      | _ -> raise VoidValue)
  | POSTFIX_EXPR (x, pop) ->
      let n = read_var x in
      update_var x
        (match pop with
        | INCR -> n + 1
        | DECR -> n - 1);
      Some n
  | _ -> None

and eval_program = function
  | VARDECL id ->
      add_var id;
      None
  | VARDECL_INIT (id, e) -> (
      match eval_expr e with
      | Some n ->
          add_var id ~init:n;
          None
      | None -> failwith "void initializer")
  | FUNDECL (id, pars, s) ->
      add_fun id (pars, remove_block s);
      None
  | IF (e, s) -> (
      match eval_expr e with
      | None -> raise VoidValue
      | Some 0 -> None
      | _ -> eval_program s)
  | IFE (e, s1, s2) -> (
      match eval_expr e with
      | None -> raise VoidValue
      | Some 0 -> eval_program s2
      | _ -> eval_program s1)
  | WHILE (e, s) ->
      add_frame ();
      let o =
        match eval_expr e with
        | None -> raise VoidValue
        | Some 0 -> None
        | _ -> eval_program (SEQ (s, WHILE_EXEC (e, remove_block s, e)))
      in
      ();
      ignore (pop_frame ());
      o
  | WHILE_EXEC (e, s, g) -> (
      match eval_expr e with
      | None -> raise VoidValue
      | Some 0 -> None
      | _ -> eval_program (SEQ (s, WHILE_EXEC (e, s, g))))
  | BLOCK s ->
      add_frame ();
      let o = eval_program s in
      ();
      ignore (pop_frame ());
      o
  | EXPR e ->
      ignore (eval_expr e);
      None
  | RET e | SEQ (RET e, _) ->
      Option.bind e (fun e ->
          let v = eval_expr e in
          if Option.is_none v then raise VoidValue;
          v)
  | SEQ (s1, s2) ->
      ignore (eval_program s1);
      eval_program s2
  | _ -> None

and eval p =
  init ();
  ignore (eval_program p);
  eval_expr (CALL ("main", []))
