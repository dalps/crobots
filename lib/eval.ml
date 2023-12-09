open Ast
open Memory

let fun_of_uop = function
  | UMinus -> ( ~- )

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
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )
  | Mod -> ( mod )
  | Eq -> int_eq
  | Neq -> int_neq
  | Gt -> int_gt
  | Lt -> int_lt
  | Geq -> int_geq
  | Leq -> int_leq
  | Land -> int_and
  | Lor -> int_or

exception VoidValue

let rec eval_expr = function
  | Int_const n -> Some n
  | Var x -> (
      match find_env envrmt x with
      | Loc l -> Some (find_mem l)
      | Null -> failwith (Printf.sprintf "variable %s declared but undefined" x)
      | _ -> failwith "expected an int variable")
  | Assign_exp (x, e) -> (
      match eval_expr e with
      | Some n ->
          bind x n;
          Some n
      | None -> raise VoidValue)
  | Call_exp (f, es) ->
      add_frame envrmt;
      let o =
        match find_env envrmt f with
        | Fun (pars, s) ->
            List.iter2
              (fun x e ->
                match eval_expr e with
                | Some n ->
                    add_env envrmt x Null;
                    bind x n
                | None -> raise VoidValue)
              pars es;
            eval_program s
        | _ -> failwith "expected a functional value"
      in
      ();
      ignore (pop_frame envrmt);
      o
  | Unary_exp (uop, e) -> (
      match eval_expr e with
      | Some n -> Some ((fun_of_uop uop) n)
      | None -> raise VoidValue)
  | Binary_exp (bop, e1, e2) -> (
      match (eval_expr e1, eval_expr e2) with
      | Some n1, Some n2 -> Some ((fun_of_bop bop) n1 n2)
      | _ -> raise VoidValue)
  | Postfix_exp (op, x) -> (
      match find_env envrmt x with
      | Loc l ->
          let n = find_mem l in
          bind x
            (match op with
            | Incr -> n + 1
            | Decr -> n - 1);
          Some n
      | Null ->
          failwith
            (Printf.sprintf "cannot increment/decrement undefined variable %s" x)
      | _ -> failwith "expected an int variable")
  | _ -> None

and eval_program = function
  | Decl_var id ->
      add_env envrmt id Null;
      None
  | Decl_var_init (id, e) -> (
      match eval_expr e with
      | Some n ->
          add_env envrmt id Null;
          bind id n;
          None
      | None -> failwith "void initializer")
  | Decl_fun (id, pars, s) ->
      add_env envrmt id (Fun (pars, remove_compoundstat s));
      None
  | If (e, s) -> (
      match eval_expr e with
      | None -> raise VoidValue
      | Some 0 -> None
      | _ -> eval_program s)
  | If_else (e, s1, s2) -> (
      match eval_expr e with
      | None -> raise VoidValue
      | Some 0 -> eval_program s2
      | _ -> eval_program s1)
  | While (e, s) ->
      add_frame envrmt;
      let o =
        match eval_expr e with
        | None -> raise VoidValue
        | Some 0 -> None
        | _ -> eval_program (Seq (s, While_exec (e, remove_compoundstat s)))
      in
      ();
      ignore (pop_frame envrmt);
      o
  | While_exec (e, s) -> (
      match eval_expr e with
      | None -> raise VoidValue
      | Some 0 -> None
      | _ -> eval_program (Seq (s, While_exec (e, s))))
  | Compound_stat s ->
      add_frame envrmt;
      let o = eval_program s in
      ();
      ignore (pop_frame envrmt);
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

and eval p =
  init ();
  ignore (eval_program p);
  eval_expr (Call_exp ("main", []))
