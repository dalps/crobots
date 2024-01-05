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
let scan_height = 70.
let explosion_radius = 50.

let colors = Raylib.[| Color.blue; Color.brown; Color.darkgreen; Color.pink |]

(* flip coordinates on the x axis, scale down by 1/click and translate on the y axis *)
let get_screen_x x = (x / Robot.click) + padding
let get_screen_x_f x = get_screen_x x |> float_of_int
let get_screen_y y = (-1 * y / Robot.click) + arena_width + padding
let get_screen_y_f y = get_screen_y y |> float_of_int
let get_screen_degrees d = -d + 270
let get_screen_degrees_f d = get_screen_degrees d |> float_of_int

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
    turret = Rectangle.create init_x init_y turret_width turret_width;
    cannon = Rectangle.create init_x init_y cannon_width cannon_height;
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
  let x = get_screen_x_f r.x in
  let y = get_screen_y_f r.y in
  Rectangle.set_x s.tank x;
  Rectangle.set_x s.turret x;
  Rectangle.set_x s.cannon x;
  Rectangle.set_y s.tank y;
  Rectangle.set_y s.turret y;
  Rectangle.set_y s.cannon y;
  Array.iter2
    (fun sprite (m : Missile.t) ->
      let x = get_screen_x_f m.cur_x in
      let y = get_screen_y_f m.cur_y in
      Rectangle.set_x sprite x;
      Rectangle.set_y sprite y)
    s.missiles r.missiles

let draw_sprite (s : t) (r : Robot.t) =
  let color_turret = color_brightness s.color 0.25 in
  let color_cannon = color_brightness s.color 0.5 in

  (* draw any flying or exploding missile *)
  Array.iteri
    (fun i (m : Missile.t) ->
      match m.status with
      | FLYING ->
          draw_rectangle_pro s.missiles.(i)
            (Vector2.create (missile_width /. 2.) (missile_height /. 2.))
            (get_screen_degrees_f m.heading)
            Color.black
      | EXPLODING ->
          let x = get_screen_x m.cur_x in
          let y = get_screen_y m.cur_y in
          draw_circle x y explosion_radius (fade Color.yellow 0.5)
      | _ -> ())
    r.missiles;

  match r.status with
  | ALIVE ->
      (* draw the scan radar *)
      let res = r.scan_res |> float_of_int in
      let dir = r.scan_degrees + 90 |> float_of_int in
      let x = get_screen_x r.x in
      let y = get_screen_y r.y in
      if r.scan_cycles > 0 then (
        draw_circle_sector
          (Vector2.create (x |> float_of_int) (y |> float_of_int))
          scan_height (dir +. res) (dir -. res) 1 (fade Color.red 0.1);
        draw_circle_lines x y scan_height (fade Color.red 0.1));

      (* draw the tank, the cannon and the turret *)
      draw_rectangle_pro s.tank
        (Vector2.create (tank_width /. 2.) (tank_width /. 2.))
        (get_screen_degrees_f r.heading)
        s.color;

      draw_rectangle_pro s.cannon
        (Vector2.create (cannon_width /. 2.) (cannon_width /. 2.))
        (get_screen_degrees_f r.turret_heading)
        color_cannon;

      draw_rectangle_pro s.turret
        (Vector2.create (turret_width /. 2.) (turret_width /. 2.))
        (get_screen_degrees_f r.turret_heading)
        color_turret;

      (* draw the robot's name floating around its sprite *)
      let fontsize = 20. in
      let v = measure_text_ex !stat_font r.name fontsize font_spacing in
      draw_stat_text_s r.name
        (let vx = Vector2.x v |> int_of_float in
         let x' = x - (vx / 2) in
         if x' < padding then padding
         else if x' + vx > arena_width + padding then arena_width + padding - vx
         else x')
        (let vy = Vector2.y v |> int_of_float in
         let offset = 55 in
         let y' = y + offset in
         if y' + vy > arena_width + padding - offset then
           arena_width + padding - vy - offset
         else y')
        fontsize Color.gray
  | DEAD -> ()
