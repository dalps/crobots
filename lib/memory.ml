open Ast

type memval = Null | Int of int | Code of (parameters * instruction)
type t = (string, memval) Hashtbl.t Stack.t

let memory : t =
  let s = Stack.create () in
  Stack.push (Hashtbl.create 99) s;
  s

let mem mem = Hashtbl.mem (Stack.top mem)
let find mem = Hashtbl.find (Stack.top mem)
let add mem = Hashtbl.add (Stack.top mem)
let add_frame mem = Stack.push (Stack.top mem) mem
let pop_frame = Stack.pop
