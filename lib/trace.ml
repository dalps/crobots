open Ast
open Robot
open Memory

exception NoRuleApplies

exception WrongArguments of int * int

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

let apply0 f = function
  | [] -> f ()
  | l -> raise (WrongArguments (List.length l, 0))
let apply1 f = function
  | [ x ] -> f x
  | l -> raise (WrongArguments (List.length l, 1))
let apply2 f = function
  | [ x; y ] -> f x y
  | l -> raise (WrongArguments (List.length l, 2))

let apply_intrinsic args = function
  | SCAN -> Some (apply2 scan args)
  | CANNON -> Some (apply2 cannon args)
  | DRIVE ->
      apply2 drive args;
      None
  | DAMAGE -> Some (apply0 damage args)
  | SPEED -> Some (apply0 speed args)
  | HEADING -> Some (apply0 heading args)
  | LOC_X -> Some (apply0 loc_x args)
  | LOC_Y -> Some (apply0 loc_y args)
  | RAND -> Some (apply1 rand args)
  | SQRT -> Some (apply1 sqrt args)
  | SIN -> Some (apply1 sin args)
  | COS -> Some (apply1 cos args)
  | TAN -> Some (apply1 tan args)
  | ATAN -> Some (apply1 atan args)

type state = env_stack * memory
type conf = St | Ret of int | Instr of instruction

let call_fun (env, mem) vals f =
  match find_env env f with
  | Fun (pars, instr) -> (
      try
        add_topenv env;
        List.iter2
          (fun x -> function
            | CONST n -> add_var env mem ~init:n x
            | _ -> failwith "expected a value")
          pars vals;
        CALL_EXEC instr
      with _ -> raise (WrongArguments (List.length vals, List.length pars)))
  | Intrinsic i -> (
      let vals =
        List.map
          (function
            | CONST n -> n
            | _ -> failwith "needs further reduction")
          vals
      in
      match apply_intrinsic vals i with
      | None -> NIL
      | Some n -> CONST n)
  | _ -> failwith (Printf.sprintf "%s is not a function" f)

let rec trace_args st vals f = function
  | [] -> call_fun st vals f
  | (CONST _ as v) :: args' -> trace_args st (vals @ [ v ]) f args'
  | e :: args' ->
      let e' = trace1_expr st e in
      CALL (f, vals @ (e' :: args'))

and trace1_expr ((env, mem) as st) e =
  match e with
  | IDE x -> CONST (read_var env mem x)
  | ASSIGN (x, (CONST n as e)) ->
      update_var env mem x n;
      e
  | ASSIGN (x, e) ->
      let e' = trace1_expr st e in
      ASSIGN (x, e')
  | CALL (f, es) -> trace_args st [] f es
  | CALL_EXEC s -> (
      match trace1_instr st (Instr s) with
      | St ->
          ignore (pop_frame env);
          NIL
      | Ret n ->
          ignore (pop_frame env);
          CONST n
      | Instr s' -> CALL_EXEC s')
  | UNARY_EXPR (uop, CONST n) -> CONST ((fun_of_uop uop) n)
  | UNARY_EXPR (uop, e) ->
      let e' = trace1_expr st e in
      UNARY_EXPR (uop, e')
  | BINARY_EXPR (CONST n1, bop, CONST n2) -> CONST ((fun_of_bop bop) n1 n2)
  | BINARY_EXPR ((CONST _ as e1), bop, e2) ->
      let e2' = trace1_expr st e2 in
      BINARY_EXPR (e1, bop, e2')
  | BINARY_EXPR (e1, bop, e2) ->
      let e1' = trace1_expr st e1 in
      BINARY_EXPR (e1', bop, e2)
  | _ -> raise NoRuleApplies

and trace1_instr ((env, mem) as st) s =
  match s with
  | St | Ret _ -> raise NoRuleApplies
  | Instr s -> (
      match s with
      | VARDECL id ->
          add_var env mem id;
          St
      | VARDECL_INIT (id, CONST n) ->
          add_var env mem ~init:n id;
          St
      | VARDECL_INIT (id, e) ->
          let e' = trace1_expr st e in
          Instr (VARDECL_INIT (id, e'))
      | FUNDECL (id, pars, s) ->
          add_fun env id (pars, s);
          St
      | IF (CONST 0, _) -> St
      | IF (CONST _, s) -> Instr s
      | IF (e, s) ->
          let e' = trace1_expr st e in
          Instr (IF (e', s))
      | IFE (CONST 0, _, s2) -> Instr s2
      | IFE (CONST _, s1, _) -> Instr s1
      | IFE (e, s1, s2) ->
          let e' = trace1_expr st e in
          Instr (IFE (e', s1, s2))
      | WHILE (e, s) -> Instr (WHILE_EXEC (e, s, e)) |> trace1_instr st
      | WHILE_EXEC (CONST 0, _, _) -> St
      | WHILE_EXEC (CONST _, s, g) -> Instr (SEQ (s, WHILE_EXEC (g, s, g)))
      | WHILE_EXEC (e, s, g) ->
          let e' = trace1_expr st e in
          Instr (WHILE_EXEC (e', s, g))
      | BLOCK s ->
          add_topenv env;
          Instr (BLOCK_EXEC s) |> trace1_instr st
      | BLOCK_EXEC s -> (
          match trace1_instr st (Instr s) with
          | Instr s' -> Instr (BLOCK_EXEC s')
          | Ret n ->
              ignore (pop_frame env);
              Ret n
          | St ->
              ignore (pop_frame env);
              St)
      | RET o ->
          Option.fold o ~none:St ~some:(function
            | CONST n -> Ret n
            | e ->
                let e' = trace1_expr st e in
                Instr (RET (Some e')))
      | EXPR (CONST _) | EXPR NIL -> St
      | EXPR e ->
          let e' = trace1_expr st e in
          Instr (EXPR e')
      | SEQ (s1, s2) -> (
          match trace1_instr st (Instr s1) with
          | Instr s1' -> Instr (SEQ (s1', s2))
          | St -> Instr s2
          | Ret n -> Ret n)
      | _ -> St)

let rec trace_instr st conf =
  try conf :: trace_instr st (trace1_instr st conf)
  with NoRuleApplies -> [ conf ]

let rec trace_expr st e =
  try e :: trace_expr st (trace1_expr st e) with NoRuleApplies -> [ e ]

let rec trace_instr_st ((env, mem) as st) conf =
  let env, mem = (get_env env, get_mem mem) in
  try (env, mem, conf) :: trace_instr_st st (trace1_instr st conf)
  with NoRuleApplies -> [ (env, mem, conf) ]

and trace_expr_st ((env, mem) as st) e =
  let env, mem = (get_env env, get_mem mem) in
  try (env, mem, e) :: trace_expr_st st (trace1_expr st e)
  with NoRuleApplies -> [ (env, mem, e) ]

let mem0 = init_memory ()
let env0 = init_stack ()
let st0 = (env0, mem0)

let trace s =
  let conf0 = Instr s in
  ignore (trace_instr st0 conf0);
  trace_expr st0 Ast.entry_point

let trace_st s =
  let conf0 = Instr s in
  ignore (trace_instr st0 conf0);
  trace_expr_st st0 Ast.entry_point
