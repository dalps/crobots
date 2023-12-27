open Raylib

let window_width = 1000
let window_height = 1000
let robot_width = 50.
let robot_height = 50.
let tank_width = robot_width
let turret_width = tank_width *. 0.7
let cannon_width = tank_width *. 0.2
let cannon_height = tank_width *. 1.2
let camera = Camera2D.create (Vector2.create 0. 0.) (Vector2.create 0. 0.) 0. 1.

let colors = Raylib.[| Color.blue; Color.brown; Color.darkgreen; Color.pink |]

type t = {
  tank : Rectangle.t;
  turret : Rectangle.t;
  cannon : Rectangle.t;
  color : Color.t;
}

let create init_x init_y color =
  {
    tank = Rectangle.create init_x init_y tank_width tank_width;
    turret =
      Rectangle.create
        (init_x +. ((tank_width -. turret_width) /. 2.))
        (init_y +. ((tank_width -. turret_width) /. 2.))
        turret_width turret_width;
    cannon =
      Rectangle.create
        (init_x +. ((tank_width -. cannon_width) /. 2.))
        (init_y +. ((tank_width -. cannon_height) /. 2.))
        cannon_width cannon_height;
    color;
  }

let sprites =
  Array.make 4
    (create
       ((window_width |> float_of_int) /. 2.)
       ((window_height |> float_of_int) /. 2.)
       Raylib.Color.black)

let set_x (s : t) (x : float) =
  Rectangle.set_x s.tank x;
  Rectangle.set_x s.turret x;
  Rectangle.set_x s.cannon x

let set_y (s : t) (y : float) =
  Rectangle.set_y s.tank y;
  Rectangle.set_y s.turret y;
  Rectangle.set_y s.cannon y

let draw_sprite (s : t) (heading : float) (scan : float) =
  let c_turret = color_brightness s.color 0.25 in
  let c_cannon = color_brightness s.color 0.25 in
  draw_rectangle_pro s.tank
    (Vector2.create (tank_width /. 2.) (tank_width /. 2.))
    heading s.color;
  draw_rectangle_pro s.cannon
    (Vector2.create
       ((tank_width /. 2.)
       -. ((tank_width -. turret_width) /. 2.)
       -. ((turret_width -. cannon_width) /. 2.))
       ((tank_width /. 2.)
       -. ((tank_width -. turret_width) /. 2.)
       -. ((turret_width -. cannon_width) /. 2.)))
    scan c_cannon;
  draw_rectangle_pro s.turret
    (Vector2.create
       ((tank_width /. 2.) -. ((tank_width -. turret_width) /. 2.))
       ((tank_width /. 2.) -. ((tank_width -. turret_width) /. 2.)))
    scan c_turret
