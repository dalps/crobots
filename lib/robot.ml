open Ast
open Memory

let clicks = 1
let max_x = 1000
let max_y = 1000
let accel = 1
let turn_speed = 50
let robot_speed = 1
let collision = 5

type status = ALIVE | DEAD

type t = {
  mutable status : status;
  mutable name : string;
  mutable x : int;
  mutable y : int;
  mutable org_x : int;
  mutable org_y : int;
  mutable range : int;
  mutable last_x : int;
  mutable last_y : int;
  mutable damage : int;
  mutable last_damage : int;
  mutable speed : int;
  mutable last_speed : int;
  mutable d_speed : int;
  mutable accel : int;
  mutable heading : int;
  mutable last_heading : int;
  mutable d_heading : int;
  mutable scan_degrees : int;
  mutable reload : bool;
  mutable program : program;
  mutable ep : expression;
  mutable env : environment;
  mutable mem : memory;
}

let init () =
  {
    status = ALIVE;
    name = "foo";
    x = 0;
    y = 0;
    org_x = 0;
    org_y = 0;
    range = 0;
    last_x = 0;
    last_y = 0;
    damage = 0;
    last_damage = 0;
    speed = 0;
    last_speed = 0;
    d_speed = 0;
    accel = 0;
    heading = 0;
    last_heading = 0;
    d_heading = 0;
    scan_degrees = 0;
    reload = false;
    program = EMPTY;
    ep = NIL;
    env = Stack.create ();
    mem = Hashtbl.create 0;
  }

let cur_robot = init ()

let degree_of_int d = abs d mod 360
let perc_of_int n = max 0 n |> min 100

let scan = ( + )
let cannon _ _ = 0

let drive degree speed =
  cur_robot.d_heading <- degree_of_int degree;
  cur_robot.d_speed <- perc_of_int speed

let damage () = cur_robot.damage
let speed () = cur_robot.speed
let loc_x () = cur_robot.x
let loc_y () = cur_robot.y

let rand = Random.int
let sqrt x = abs x |> float_of_int |> Float.sqrt |> int_of_float

let int_trig ?(fact = 1) f x =
  degree_of_int x |> float_of_int
  |> (fun x -> x *. Float.pi /. 180.)
  |> f
  |> ( *. ) (float_of_int fact)
  |> Float.round |> int_of_float

let sin = int_trig Float.sin ~fact:100_000
let cos = int_trig Float.cos ~fact:100_000
let tan = int_trig Float.tan ~fact:100_000
let atan x =
  x |> float_of_int
  |> ( *. ) (1. /. 100_000.)
  |> Float.atan
  |> (fun x -> x *. 180. /. Float.pi)
  |> int_of_float

let update_robot (r : t) =
  (* update speed, moderated by acceleration *)
  (match compare r.speed r.d_speed with
  | n when n < 0 ->
      (* accelerating *)
      r.accel <- r.accel + accel;
      if r.accel > r.d_speed then r.accel <- r.d_speed;
      r.speed <- r.accel
  | 0 -> ()
  | _ ->
      (* slowing *)
      r.accel <- r.accel - r.accel;
      if r.accel < r.d_speed then r.accel <- r.d_speed;
      r.speed <- r.accel);

  (* update heading *)
  if r.heading != r.d_heading then
    if r.speed <= turn_speed then (
      r.heading <- r.d_heading;
      r.range <- 0;
      r.org_y <- r.y;
      r.org_x <- r.x)
    else r.d_speed <- 0;

  (* update distance traveled on this heading and position *)
  if r.speed > 0 then (
    r.range <- r.range + (r.speed / clicks * robot_speed);
    r.x <- r.org_x + (cos r.heading * (r.range / clicks) / 100000);
    r.y <- r.org_y + (sin r.heading * (r.range / clicks) / 100000);

    (* check for collisions with a wall *)
    (match r.x with
    | x when x < 0 ->
        Printf.printf "%s hit west wall\n" r.name;
        r.x <- 0;
        r.speed <- 0;
        r.d_speed <- 0;
        r.damage <- r.damage + collision
    | x when x > max_x * clicks ->
        Printf.printf "%s hit east wall\n" r.name;
        r.x <- (max_x * clicks) - 1;
        r.speed <- 0;
        r.d_speed <- 0;
        r.damage <- r.damage + collision
    | _ -> ());

    match r.y with
    | y when y < 0 ->
        Printf.printf "%s hit south wall\n" r.name;
        r.y <- 0;
        r.speed <- 0;
        r.d_speed <- 0;
        r.damage <- r.damage + collision
    | y when y > max_y * clicks ->
        Printf.printf "%s hit north wall\n" r.name;
        r.y <- (max_y * clicks) - 1;
        r.speed <- 0;
        r.d_speed <- 0;
        r.damage <- r.damage + collision
    | _ -> ())
