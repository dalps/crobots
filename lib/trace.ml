open Ast
open Memory
open Eval

exception NoRuleApplies

type state = environment * memory
type conf = St | Ret of int | Instr of instruction

let rec trace_args vals f = function
  | [] ->
      add_frame ();
      let pars, instr = read_fun f in
      List.iter2
        (fun x -> function
          | CONST n -> add_var ~init:n x
          | _ -> failwith "expected a value")
        pars vals;
      CALL_EXEC instr
  | (CONST _ as v) :: args' -> trace_args (vals @ [ v ]) f args'
  | e :: args' ->
      let e' = trace1_expr e in
      CALL (f, vals @ (e' :: args'))

and trace1_expr = function
  | IDE x -> CONST (read_var x)
  | ASSIGN (x, (CONST n as e)) ->
      update_var x n;
      e
  | ASSIGN (x, e) ->
      let e' = trace1_expr e in
      ASSIGN (x, e')
  | CALL (f, es) -> trace_args [] f es
  | CALL_EXEC s -> (
      match trace1_instr (Instr s) with
      | St -> NIL
      | Ret n -> CONST n
      | Instr s' -> CALL_EXEC s')
  | UNARY_EXPR (uop, CONST n) -> CONST ((fun_of_uop uop) n)
  | UNARY_EXPR (uop, e) ->
      let e' = trace1_expr e in
      UNARY_EXPR (uop, e')
  | BINARY_EXPR (CONST n1, bop, CONST n2) -> CONST ((fun_of_bop bop) n1 n2)
  | BINARY_EXPR ((CONST _ as e1), bop, e2) ->
      let e2' = trace1_expr e2 in
      BINARY_EXPR (e1, bop, e2')
  | BINARY_EXPR (e1, bop, e2) ->
      let e1' = trace1_expr e1 in
      BINARY_EXPR (e1', bop, e2)
  | _ -> raise NoRuleApplies

and trace1_instr = function
  | St | Ret _ -> raise NoRuleApplies
  | Instr s -> (
      match s with
      | VARDECL id ->
          add_var id;
          St
      | VARDECL_INIT (id, CONST n) ->
          add_var ~init:n id;
          St
      | VARDECL_INIT (id, e) ->
          let e' = trace1_expr e in
          Instr (VARDECL_INIT (id, e'))
      | FUNDECL (id, pars, s) ->
          add_fun id (pars, s);
          St
      | IF (CONST 0, _) -> St
      | IF (CONST _, s) -> Instr s
      | IF (e, s) ->
          let e' = trace1_expr e in
          Instr (IF (e', s))
      | IFE (CONST 0, _, s2) -> Instr s2
      | IFE (CONST _, s1, _) -> Instr s1
      | IFE (e, s1, s2) ->
          let e' = trace1_expr e in
          Instr (IFE (e', s1, s2))
      | WHILE (e, s) -> Instr (WHILE_EXEC (e, s, e)) |> trace1_instr
      | WHILE_EXEC (CONST 0, _, _) -> St
      | WHILE_EXEC (CONST _, s, g) -> Instr (SEQ (s, WHILE_EXEC (g, s, g)))
      | WHILE_EXEC (e, s, g) ->
          let e' = trace1_expr e in
          Instr (WHILE_EXEC (e', s, g))
      | BLOCK s ->
          add_frame ();
          Instr (BLOCK_EXEC s) |> trace1_instr
      | BLOCK_EXEC s -> (
          match trace1_instr (Instr s) with
          | Instr s' -> Instr (BLOCK_EXEC s')
          | Ret n -> Ret n
          | St ->
              ignore (pop_frame ());
              St)
      | RET o ->
          Option.fold o ~none:St ~some:(function
            | CONST n ->
                ignore (pop_frame ());
                Ret n
            | e ->
                let e' = trace1_expr e in
                Instr (RET (Some e')))
      | EXPR (CONST _) | EXPR NIL -> St
      | EXPR e ->
          let e' = trace1_expr e in
          Instr (EXPR e')
      | SEQ (s1, s2) -> (
          match trace1_instr (Instr s1) with
          | Instr s1' -> Instr (SEQ (s1', s2))
          | St -> Instr s2
          | Ret n -> Ret n)
      | _ -> raise NoRuleApplies)

let rec trace_instr conf =
  try conf :: trace_instr (trace1_instr conf) with NoRuleApplies -> [ conf ]

and trace_expr e =
  try e :: trace_expr (trace1_expr e) with NoRuleApplies -> [ e ]

let trace s =
  init ();
  let conf0 = Instr s in
  ignore (trace_instr conf0);
  trace_expr (CALL ("main", []))

let rec trace_instr_st conf =
  try
    let env = get_env () in
    let mem = get_mem () in
    (env, mem, conf) :: trace_instr_st (trace1_instr conf)
  with NoRuleApplies -> [ (get_env (), get_mem (), conf) ]

and trace_expr_st e =
  try
    let env = get_env () in
    let mem = get_mem () in
    (env, mem, e) :: trace_expr_st (trace1_expr e)
  with NoRuleApplies -> [ (get_env (), get_mem (), e) ]

let trace_st s =
  init ();
  let conf0 = Instr s in
  ignore (trace_instr conf0);
  trace_expr_st (CALL ("main", []))
