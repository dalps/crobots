open Ast

exception IntrinsicOverride
exception WrongArguments of int * int

exception UndeclaredVariable of string
exception UndefinedVariable of string

type loc = int
type ide = identifier

type intrinsic =
  | SCAN
  | CANNON
  | DRIVE
  | DAMAGE
  | SPEED
  | LOC_X
  | LOC_Y
  | RAND
  | SQRT
  | SIN
  | COS
  | TAN
  | ATAN

type memval = int
type envval =
  | Loc of loc
  | Fun of (parameters * instruction)
  | Intrinsic of intrinsic

type memory = (loc, memval) Hashtbl.t
type environment = (ide, envval) Hashtbl.t Stack.t

let memory : memory = Hashtbl.create 99

let envrmt : environment = Stack.create ()

let max_key h =
  Hashtbl.fold
    (fun k _ m ->
      match m with
      | None -> Some k
      | Some m -> Some (max k m))
    h None

let fresh_loc () = Option.value ~default:0 (max_key memory) + 1

let get_mem () = Hashtbl.copy memory
let find_mem = Hashtbl.find memory
let add_mem = Hashtbl.add memory
let update_mem = Hashtbl.replace memory

let get_env () = Hashtbl.copy (Stack.top envrmt)
let find_env env x =
  try Hashtbl.find (Stack.top env) x
  with Not_found -> raise (UndeclaredVariable x)
let add_env env = Hashtbl.add (Stack.top env)

let add_frame () = Stack.push (Stack.top envrmt |> Hashtbl.copy) envrmt
let pop_frame () = Stack.pop envrmt

let init () =
  Hashtbl.reset memory;
  Stack.clear envrmt;
  Stack.push (Hashtbl.create 99) envrmt;
  add_env envrmt "scan" (Intrinsic SCAN);
  add_env envrmt "cannon" (Intrinsic CANNON);
  add_env envrmt "drive" (Intrinsic DRIVE);
  add_env envrmt "damage" (Intrinsic DAMAGE);
  add_env envrmt "speed" (Intrinsic SPEED);
  add_env envrmt "loc_x" (Intrinsic LOC_X);
  add_env envrmt "loc_y" (Intrinsic LOC_Y);
  add_env envrmt "rand" (Intrinsic RAND);
  add_env envrmt "sqrt" (Intrinsic SQRT);
  add_env envrmt "sin" (Intrinsic SIN);
  add_env envrmt "cos" (Intrinsic COS);
  add_env envrmt "tan" (Intrinsic TAN);
  add_env envrmt "atan" (Intrinsic ATAN)

let add_var ?(init = 0) ide =
  let l = fresh_loc () in
  add_mem l init;
  add_env envrmt ide (Loc l)

let read_var x =
  match find_env envrmt x with
  | Loc l -> find_mem l
  | _ -> failwith "read_var on function"

let update_var x n =
  match find_env envrmt x with
  | Loc l -> update_mem l n
  | _ -> failwith "update_var on function"

let add_fun ide (pars, s) =
  match ide with
  | "scan" | "cannon" | "drive" | "damage" | "speed" | "loc_x" | "loc_y"
  | "rand" | "sqrt" | "sin" | "cos" | "tan" | "atan" ->
      raise IntrinsicOverride
  | _ -> add_env envrmt ide (Fun (pars, s))

let read_fun x =
  match find_env envrmt x with
  | Fun (pars, s) -> (pars, s)
  | _ -> failwith "read_fun on integer"

let janitor () =
  let all_locs = Hashtbl.to_seq_keys memory |> List.of_seq in
  let used_locs =
    Hashtbl.fold
      (fun (_ : ide) (v : envval) seq ->
        match v with
        | Loc l -> l :: seq
        | _ -> seq)
      (Stack.top envrmt) []
  in
  List.iter
    (fun l -> if not (List.mem l used_locs) then Hashtbl.remove memory l)
    all_locs

let apply0 f = function
  | [] -> f ()
  | l -> raise (WrongArguments (0, List.length l))
let apply1 f = function
  | [ x ] -> f x
  | l -> raise (WrongArguments (1, List.length l))
let apply2 f = function
  | [ x; y ] -> f x y
  | l -> raise (WrongArguments (2, List.length l))

let apply_intrinsic args = function
  | SCAN -> Some (apply2 Robot.scan args)
  | CANNON -> Some (apply2 Robot.cannon args)
  | DRIVE ->
      apply2 Robot.drive args;
      None
  | DAMAGE -> Some (apply0 Robot.damage args)
  | SPEED -> Some (apply0 Robot.speed args)
  | LOC_X -> Some (apply0 Robot.loc_x args)
  | LOC_Y -> Some (apply0 Robot.loc_y args)
  | RAND -> Some (apply1 Robot.rand args)
  | SQRT -> Some (apply1 Robot.sqrt args)
  | SIN -> Some (apply1 Robot.sin args)
  | COS -> Some (apply1 Robot.cos args)
  | TAN -> Some (apply1 Robot.tan args)
  | ATAN -> Some (apply1 Robot.sin args)
