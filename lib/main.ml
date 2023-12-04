open Ast
open Memory

let parse parser text =
  let lexbuf = Lexing.from_string text in
  try parser Lexer.read_token lexbuf
  with exn ->
    let pos = lexbuf.lex_curr_p
    and errstr =
      match exn with
      | Lexer.Error msg -> msg
      | Parser.Error -> "syntax error"
      | _ -> "weird error"
    in
    failwith
      (Printf.sprintf "line %d, column %d: %s%!" pos.pos_lnum
         (pos.pos_cnum - pos.pos_bol)
         errstr)

let parse_program = parse Parser.main

let fun_of_uop = function UMinus -> ( ~- )

let fun_of_bop = function
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )
  | Mod -> ( mod )

exception VoidValue

(*
  Evaluates void and int expressions
  Raises VoidValue when a void function is called in an int expression
*)
let rec eval_expr = function
  | Int_const n -> Some n
  | Var x -> (
      try
        match find memory x with
        | Int n -> Some n
        | _ -> failwith "expected an int variable"
      with Not_found -> failwith (Printf.sprintf "'%s' undeclared" x))
  | Assign_exp (x, e) -> (
      match eval_expr e with
      | Some n ->
          if mem memory x then (
            add memory x (Int n);
            Some n)
          else failwith (Printf.sprintf "'%s' undeclared" x)
      | None -> raise VoidValue)
  | Call_exp (f, es) -> (
      match find memory f with
      | Code (pars, s) ->
          List.iter2
            (fun x e ->
              match eval_expr e with
              | Some n -> add memory x (Int n)
              | None -> raise VoidValue)
            pars es;
          eval_program s
      | _ -> failwith "expected a functional value")
  | Unary_exp (uop, e) -> (
      match eval_expr e with
      | Some n -> Some ((fun_of_uop uop) n)
      | None -> raise VoidValue)
  | Binary_exp (bop, e1, e2) -> (
      match (eval_expr e1, eval_expr e2) with
      | Some n1, Some n2 -> Some ((fun_of_bop bop) n1 n2)
      | _ -> raise VoidValue)

and eval_program = function
  | Decl_var id ->
      add memory id Null;
      None
  | Decl_var_init (id, e) ->
      Option.bind (eval_expr e) (fun n ->
          add memory id (Int n);
          None)
  | Decl_fun (id, pars, s) ->
      add memory id (Code (pars, s));
      None
  | If (e, s) ->
      let v = eval_expr e in
      if Option.is_none v then raise VoidValue;
      if v = Some 0 then None else eval_program s
  | If_else (e, s1, s2) ->
      let v = eval_expr e in
      if Option.is_none v then raise VoidValue;
      if v = Some 0 then eval_program s2 else eval_program s1
  | Compound_stat s ->
      add_frame memory;
      let o = eval_program s in
      ();
      ignore (pop_frame memory);
      o
  | Exp_stat e ->
      ignore (eval_expr e);
      None
  | Return_stat e | Seq (Return_stat e, _) ->
      Option.bind e (fun e ->
          let v = eval_expr e in
          if Option.is_none v then raise VoidValue;
          v)
  | Seq (s1, s2) ->
      ignore (eval_program s1);
      eval_program s2
  | _ -> None

and eval_main p =
  ignore (eval_program p);
  eval_expr (Call_exp ("main", []))

exception NoRuleApplies

let rec trace1 = function
  | Decl_var id ->
      add memory id Null;
      raise NoRuleApplies
  | Decl_var_init (id, e) ->
      let v = eval_expr e in
      add memory id (Int (Option.get v));
      raise NoRuleApplies
  | Decl_fun (id, pars, s) ->
      add memory id (Code (pars, s));
      List.iter (fun id -> add memory id Null) pars;
      raise NoRuleApplies
  | If (e, s) when eval_expr e |> Option.get = 1 -> s
  | If_else (e, s1, s2) -> if eval_expr e |> Option.get = 0 then s2 else s1
  | Compound_stat s -> trace1 s
  | Seq (x, xlst) -> (
      try
        let x' = trace1 x in
        Seq (x', xlst)
      with NoRuleApplies -> xlst)
  | _ -> raise NoRuleApplies

let rec trace s =
  try
    let s' = trace1 s in
    s :: trace s'
  with NoRuleApplies -> [ s ]
