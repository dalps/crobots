open Ast

exception IntrinsicOverride
exception UndeclaredVariable of string

let block_tag = "__block__"
type loc = int
type ide = identifier

type memval = int
type envval =
  | Loc of loc
  | Fun of (parameters * instruction)
  | Intrinsic of intrinsic

type tag = string

type memory = (loc, memval) Hashtbl.t
type environment = (ide, envval) Hashtbl.t
type stackval = (tag * environment)
type env_stack = stackval Stack.t

let max_key h =
  Hashtbl.fold
    (fun k _ m ->
      match m with
      | None -> Some k
      | Some m -> Some (max k m))
    h None

let fresh_loc mem = Option.value ~default:0 (max_key mem) + 1

let get_mem = Hashtbl.copy
let find_mem = Hashtbl.find
let add_mem = Hashtbl.add
let update_mem = Hashtbl.replace

let top_tag env = Stack.top env |> fst
let top_frame env = Stack.top env |> snd
let add_topenv t env = Stack.push (t, top_frame env |> Hashtbl.copy) env
let add_frame f env = Stack.push f env
let pop_frame env = Stack.pop env

let rec pop_blocks env =
  let t = top_tag env in
  if (not (Stack.is_empty env)) && String.starts_with ~prefix:block_tag t then (
    Stack.pop env |> ignore;
    pop_blocks env)
  else pop_frame env

let get_env env = Hashtbl.copy (top_frame env)
let find_env env x =
  try Hashtbl.find (top_frame env) x
  with Not_found -> raise (UndeclaredVariable x)
let add_env env = Hashtbl.add (top_frame env)

let init_stack () =
  let env = Stack.create () in
  Stack.push ("", Hashtbl.create 99) env;
  add_env env "scan" (Intrinsic SCAN);
  add_env env "cannon" (Intrinsic CANNON);
  add_env env "drive" (Intrinsic DRIVE);
  add_env env "damage" (Intrinsic DAMAGE);
  add_env env "speed" (Intrinsic SPEED);
  add_env env "loc_x" (Intrinsic LOC_X);
  add_env env "loc_y" (Intrinsic LOC_Y);
  add_env env "rand" (Intrinsic RAND);
  add_env env "sqrt" (Intrinsic SQRT);
  add_env env "sin" (Intrinsic SIN);
  add_env env "cos" (Intrinsic COS);
  add_env env "tan" (Intrinsic TAN);
  add_env env "atan" (Intrinsic ATAN);
  env

let init_memory () = Hashtbl.create 99

let add_var ?(init = 0) env mem ide =
  let l = fresh_loc mem in
  add_mem mem l init;
  add_env env ide (Loc l)

let read_var env mem x =
  match find_env env x with
  | Loc l -> find_mem mem l
  | _ -> failwith "read_var on function"

let update_var env mem x n =
  match find_env env x with
  | Loc l -> update_mem mem l n
  | _ -> failwith "update_var on function"

let add_fun env ide data =
  match ide with
  | "scan" | "cannon" | "drive" | "damage" | "speed" | "loc_x" | "loc_y"
  | "rand" | "sqrt" | "sin" | "cos" | "tan" | "atan" ->
      raise IntrinsicOverride
  | _ -> add_env env ide (Fun data)

let read_fun env x =
  match find_env env x with
  | Fun data -> data
  | _ -> failwith "read_fun on integer"

let janitor env mem =
  let all_locs = Hashtbl.to_seq_keys mem |> List.of_seq in
  let used_locs =
    Hashtbl.fold
      (fun (_ : ide) (v : envval) seq ->
        match v with
        | Loc l -> l :: seq
        | _ -> seq)
      (top_frame env) []
  in
  List.iter
    (fun l -> if not (List.mem l used_locs) then Hashtbl.remove mem l)
    all_locs
