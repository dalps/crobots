open Ast

let parse line : prog =
  let linebuf = Lexing.from_string line in
  Parser.main Lexer.read_token linebuf

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

let rec trace1 = function
  | If (e, s) when eval_expr e = 1 -> s
  | If_else (e, s1, s2) -> if eval_expr e = 0 then s2 else s1
  | Compound_stat s -> s
  | Stat_list (s, slst) -> (
      try
        let s' = trace1 s in
        Stat_list (s', slst)
      with NoRuleApplies -> slst)
  | _ -> raise NoRuleApplies

let rec trace s =
  try
    let s' = trace1 s in
    s :: trace s'
  with NoRuleApplies -> [ s ]
