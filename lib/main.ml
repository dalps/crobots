open Ast

let parse text =
  let lexbuf = Lexing.from_string text in
  try Parser.main Lexer.read_token lexbuf
  with exn ->
    let pos = lexbuf.lex_curr_p
    and errstr =
      match exn with
      | Lexer.Error msg -> msg
      | Parser.Error -> "syntax error"
      | _ -> "weird error"
    in
    raise
      (Failure
         (Printf.sprintf "line %d, column %d: %s%!" pos.pos_lnum
            (pos.pos_cnum - pos.pos_bol)
            errstr))

let fun_of_uop = function UMinus -> ( ~- )

let fun_of_bop = function
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )
  | Mod -> ( mod )

let rec eval_expr = function
  | Int_const n -> n
  | Unary_exp (uop, e) -> (fun_of_uop uop) (eval_expr e)
  | Add_exp (bop, e1, e2) | Mul_exp (bop, e1, e2) ->
      (fun_of_bop bop) (eval_expr e1) (eval_expr e2)

exception NoRuleApplies

type memval = Null | Int of int | Code of instruction
type memory = (string, memval) Hashtbl.t

let mem = Hashtbl.create 99

let rec trace1 = function
  | Decl_var id ->
      Hashtbl.add mem id Null;
      raise NoRuleApplies
  | Decl_var_init (id, e) ->
      let v = eval_expr e in
      Hashtbl.add mem id (Int v);
      raise NoRuleApplies
  | Decl_fun (id, pars, s) ->
      Hashtbl.add mem id (Code s);
      List.iter (fun id -> Hashtbl.add mem id Null) pars;
      raise NoRuleApplies
  | If (e, s) when eval_expr e = 1 -> s
  | If_else (e, s1, s2) -> if eval_expr e = 0 then s2 else s1
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
