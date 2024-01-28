exception IntrinsicOverride
(* raised when a user program attempts to redefine an intrinsic function *)

exception UndeclaredVariable of string
(* [UndeclaredVariable x] is raised when a user program attempts to use the
   undeclared variable [x] *)

type loc = int
(* the type of memory locations or addresses *)

type ide = string
(* the type of variable names *)

type memval = int
(* the type of memory items *)

type envval =
  | Loc of memval
  | Fun of (Ast.parameters * Ast.instruction)
  | Intrinsic of Ast.intrinsic
(* the type of environment items *)

type memory = (loc, memval) Hashtbl.t
(* a map from loc to memval *)

type environment = (ide, envval) Hashtbl.t
(* a map from ide to envval *)

type stackval = environment
(* the type of stack items *)

type env_stack = stackval Stack.t
(* a stack of stackval *)

(* you may expose the above four types by adding definitions *)

val init_memory : unit -> memory
(* initializes an empty memory *)

val init_stack : unit -> env_stack
(* initializes a stack with an environment defining the intrinsic functions *)

val find_mem : memory -> loc -> memval
(* memory lookup *)

val add_mem : memory -> loc -> memval -> unit
(* [add_mem mem loc n] binds the memory location [loc] to the value [n] *)

val update_mem : memory -> loc -> memval -> unit
(* [update_mem mem loc n] updates the value bound to [loc] to [n] *)

val find_env : env_stack -> ide -> envval
(* [find_env env x] reads the environemnt value bound to the name [x] in the
   current environment *)

val add_env : env_stack -> ide -> envval -> unit
(* [add_env env x v] binds the name [x] to the environment value [v] in the
   current environment *)

val add_frame : env_stack -> unit
(* pushes a copy of the top environment to the stack *)

val pop_frame : env_stack -> stackval
(* pops and returns the top environment *)

val get_env : env_stack -> environment
(* returns a copy of the top environment *)

val get_mem : memory -> memory
(* returns a copy of the memory *)

val add_var : ?init:memval -> env_stack -> memory -> ide -> unit
(* returns a copy of the memory *)

val read_var : env_stack -> memory -> ide -> memval
(* returns the memory value referenced by a variable *)

val update_var : env_stack -> memory -> ide -> memval -> unit
(* updates the memory value pointed by a variable *)

val add_fun : env_stack -> ide -> Ast.parameters * Ast.program -> unit
(* writes a function in the top environment *)

val read_fun : env_stack -> ide -> Ast.parameters * Ast.program
(* returns a function's data *)

val janitor : env_stack -> memory -> unit
(* frees unused memory locations *)
