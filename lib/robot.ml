open Ast
open Memory

type t = {
  mutable name : string;
  mutable loc_x : int;
  mutable loc_y : int;
  mutable damage : int;
  mutable speed : int;
  mutable last_speed : int;
  mutable d_speed : int;
  mutable accel : int;
  mutable heading : int;
  mutable last_heading : int;
  mutable d_heading : int;
  mutable scan_degrees : int;
  mutable reloading : bool;
  mutable program : program;
  mutable env : environment;
  mutable mem : memory;
}

let init () =
  {
    name = "foo";
    loc_x = 0;
    loc_y = 0;
    damage = 0;
    speed = 0;
    last_speed = 0;
    d_speed = 0;
    accel = 0;
    heading = 0;
    last_heading = 0;
    d_heading = 0;
    scan_degrees = 0;
    reloading = false;
    program = EMPTY;
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
let loc_x () = cur_robot.loc_x
let loc_y () = cur_robot.loc_y

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
let atan = int_trig Float.atan
