open Ast

exception UndeclaredVariable of string
exception UndefinedVariable of string

type loc = int
type ide = identifier

type memval = int

(* Environment invariant: if a name evaluates to Loc l, then l is a valid reference in memory that evaluates to a number. A memory location cannot be reserved without being recorded in memory first together with a value. *)
type envval = Null | Loc of loc | Fun of (parameters * instruction)

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

let mem_mem = Hashtbl.mem memory
let find_mem = Hashtbl.find memory
let add_mem n =
  let newloc = fresh_loc () in
  Hashtbl.add memory newloc n;
  newloc
let update_mem = Hashtbl.replace memory

let mem_env env = Hashtbl.mem (Stack.top env)
let find_env env x =
  try Hashtbl.find (Stack.top env) x
  with Not_found -> raise (UndeclaredVariable x)
let add_env env = Hashtbl.add (Stack.top env)
let add_frame env = Stack.push (Stack.top env |> Hashtbl.copy) env
let pop_frame = Stack.pop

let init () =
  Hashtbl.reset memory;
  Stack.clear envrmt;
  Stack.push (Hashtbl.create 99) envrmt

let bind x n =
  match find_env envrmt x with
  | Null ->
      let l = add_mem n in
      add_env envrmt x (Loc l)
  | Loc l -> update_mem l n
  | _ -> failwith "update on functional value"

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
