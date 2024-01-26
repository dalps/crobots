open Math

type status = ALIVE | DEAD

type t = {
  id : int;
  mutable status : status;
  mutable name : string;
  p : vector;
  dp : vector;
  mutable speed : float;
  mutable d_speed : float;
  mutable acceleration : float;
  mutable heading : float;
  mutable d_heading : float;
  mutable turret_heading : float;
  mutable d_turret_heading : float;
  mutable scan_degrees : float;
  mutable scan_cycles : int;
  mutable scan_res : float;
  mutable damage : float;
  mutable reload : int;
  mutable missiles : Missile.t array;
  mutable program : Ast.program;
  mutable ep : Ast.expression;
  mutable env : Memory.env_stack;
  mutable mem : Memory.memory;
}

val _robot_size : float
val _max_x : float
val _max_y : float
val _res_limit : float

val init : int -> string -> Ast.instruction -> float -> float -> t

val cur_robot : t ref
val all_robots : t array ref

val scan : int -> int -> int
(* invokes the robot's scanner, at a specified
   degree and resolution.
   returns 0 if no robots are within the scan
   range or a positive integer representing
   the range to the closest robot *)

val cannon : int -> int -> int
(* fires a missile heading a specified range
   and direction.
   returns 1 (true) if a missile was fired,
   or 0 (false) if the cannon is reloading *)

val drive : int -> int -> unit
(* activates the robot's drive mechanism, on
   a specified heading and speed *)

val damage : unit -> int
(* returns the current percent of damage incurred *)

val speed : unit -> int
(* returns the current percent of speed *)

val heading : unit -> int
(* returns the current heading *)

val loc_x : unit -> int
(* returns the robot's current x axis location *)

val loc_y : unit -> int
(* returns the robot's current y axis location *)

val rand : int -> int
(* returns a random number between 0 and limit *)

val sqrt : int -> int
(*returns the square root of a number, made
  positive if necessary *)

val sin : int -> int
val cos : int -> int
val tan : int -> int
(* trigonometric values.
   sin(), cos(), and tan(), take a degree
   argument, 0-359, and returns the trigonometric
   value times 100,000 *)

val atan : int -> int
(* takes a ratio argument that has been scaled up
   by 100,000, and returns a degree value,
   between -90 and +90. *)
