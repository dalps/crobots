open Raylib
open Crobots
open Gui

let robot_width = 50.
let robot_height = 50.
let tank_width = robot_width
let turret_width = tank_width *. 0.7
let cannon_width = tank_width *. 0.2
let cannon_height = tank_width *. 1.2
let missile_width = cannon_width *. 0.8
let missile_height = missile_width
let scan_height = 700.

let deg2rad = Float.pi /. 180.

let camera = Camera2D.create (Vector2.create 0. 0.) (Vector2.create 0. 0.) 0. 1.

let colors = Raylib.[| Color.blue; Color.brown; Color.darkgreen; Color.pink |]

type t = {
  tank : Rectangle.t;
  turret : Rectangle.t;
  cannon : Rectangle.t;
  missiles : Rectangle.t array;
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
    missiles =
      Array.init 2 (fun _ ->
          Rectangle.create init_x init_y missile_height missile_width);
    color;
  }

let sprites =
  Array.make 4
    (create
       ((window_width |> float_of_int) /. 2.)
       ((window_height |> float_of_int) /. 2.)
       Raylib.Color.black)

let update_sprite (s : t) (r : Robot.t) =
  let open Robot in
  let x = r.x / click in
  let y = r.y / click in
  Rectangle.set_x s.tank (padding + x |> float_of_int);
  Rectangle.set_x s.turret (padding + x |> float_of_int);
  Rectangle.set_x s.cannon (padding + x |> float_of_int);
  Rectangle.set_y s.tank (padding + y |> float_of_int);
  Rectangle.set_y s.turret (padding + y |> float_of_int);
  Rectangle.set_y s.cannon (padding + y |> float_of_int);
  Array.iter2
    (fun sprite (missile : Missile.t) ->
      let x = missile.cur_x / click in
      let y = missile.cur_y / click in
      Rectangle.set_x sprite (padding + x |> float_of_int);
      Rectangle.set_y sprite
        (padding + y
         - ((cannon_width /. 2.) -. ((cannon_width -. missile_width) /. 2.)
           |> int_of_float)
        |> float_of_int))
    s.missiles r.missiles

let draw_sprite (s : t) (r : Robot.t) =
  let color_turret = color_brightness s.color 0.25 in
  let color_cannon = color_brightness s.color 0.5 in
  let open Missile in
  Array.iteri
    (fun i (m : t) ->
      if m.status = FLYING then
        draw_rectangle_pro s.missiles.(i) (Vector2.create 0. 0.)
          (m.heading |> float_of_int)
          Color.black)
    r.missiles;
  let res = r.scan_res |> float_of_int in
  let dir = r.scan_degrees |> float_of_int in
  let l = scan_height *. Float.cos (deg2rad *. res) in
  let p = padding |> float_of_int in
  let cos x = Float.cos (deg2rad *. x) in
  let sin x = Float.sin (deg2rad *. x) in
  let x = r.x / Robot.click in
  let y = r.y / Robot.click in
  if r.scan_cycles > 0 then
    draw_triangle
      (Vector2.create (p +. (x |> float_of_int)) (p +. (y |> float_of_int)))
      (Vector2.create
         (p +. (x |> float_of_int) +. (l *. cos (dir +. res)))
         (p +. (y |> float_of_int) +. (l *. sin (dir +. res))))
      (Vector2.create
         (p +. (x |> float_of_int) +. (l *. cos (dir -. res)))
         (p +. (y |> float_of_int) +. (l *. sin (dir -. res))))
      (fade Color.red 0.1);
  (* decrease scan sprite timer; if 0 then stop drawing *)
  draw_rectangle_pro s.tank
    (Vector2.create (tank_width /. 2.) (tank_width /. 2.))
    (r.heading |> float_of_int)
    s.color;
  draw_rectangle_pro s.cannon
    (Vector2.create
       ((tank_width /. 2.)
       -. ((tank_width -. turret_width) /. 2.)
       -. ((turret_width -. cannon_width) /. 2.))
       ((tank_width /. 2.)
       -. ((tank_width -. turret_width) /. 2.)
       -. ((turret_width -. cannon_width) /. 2.)))
    (r.turret_heading - 90 |> float_of_int)
    color_cannon;
  draw_rectangle_pro s.turret
    (Vector2.create
       ((tank_width /. 2.) -. ((tank_width -. turret_width) /. 2.))
       ((tank_width /. 2.) -. ((tank_width -. turret_width) /. 2.)))
    (r.turret_heading - 90 |> float_of_int)
    color_turret
