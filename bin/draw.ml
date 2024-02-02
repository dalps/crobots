open Raylib
open Crobots
open Gui
open Sprite
open Defs

let draw_trail (s : t) =
  let srcrec_trail = get_srcrec !trail_texture in
  let origin = V.create (tank_width /. 2.) (tank_width /. 2.) in

  begin_blend_mode BlendMode.Alpha;
  PS.draw_texture s.trail !trail_texture srcrec_trail (R.width s.tank)
    trail_height origin s.color;
  end_blend_mode ()

let draw_sprite (s : t) (r : Robot.t) =
  let x = get_screen_x r.p.x in
  let y = get_screen_y r.p.y in

  let color_tank, color_turret =
    match r.status with
    | ALIVE ->
        (* draw the scan radar *)
        let res = r.scan_res in
        let dir = r.scan_degrees +. 90. in
        if r.scan_cycles > 0 then (
          draw_circle_sector (V.create x y) scan_height (dir +. res)
            (dir -. res) 1 (fade s.color 0.25);
          draw_circle_lines (x |> Float.to_int) (y |> Float.to_int) scan_height
            (fade s.color 0.25));
        (s.color, s.color)
    | DEAD ->
        let g1 = color_brightness Color.gray 0.5 in
        let g2 = color_brightness Color.gray 0.7 in
        (g1, g2)
  in

  (* draw any flying or exploding missile *)
  Array.iteri
    (fun i (m : Missile.t) ->
      match m.status with
      | FLYING ->
          draw_rectangle_pro s.missiles.(i).bullet
            (V.create (missile_width /. 2.) (missile_height /. 2.))
            (get_screen_degrees m.heading)
            bullet_color
      | _ -> ())
    r.missiles;

  (* draw the tank, the cannon and the turret *)
  Sprite.draw s r.heading r.turret_heading color_tank color_turret;

  (if r.status = DEAD then
     let skull_width = tank_width *. 0.5 in
     let dstrec = R.(create (x s.tank) (y s.tank) skull_width skull_width) in

     draw_texture_pro !skull_texture
       (get_srcrec !skull_texture)
       dstrec
       (V.create (skull_width /. 2.) (skull_width /. 2.))
       0. skull_color);

  (* draw any flying or exploding missile *)
  Array.iter
    (fun (s : missile_t) ->
      PS.draw_rec s.trail Color.gray;
      PS.draw_rec s.explosion bullet_color)
    s.missiles;

  (* draw the robot's name floating around its sprite *)
  let fontsize = 20. in
  let v = measure_text_ex !stat_font r.name fontsize font_spacing in
  let x, y =
    (x |> Float.round |> Float.to_int, y |> Float.round |> Float.to_int)
  in
  draw_stat_text_s
    (Printf.sprintf "%d.%s" r.id r.name)
    (let vx = V.x v |> int_of_float in
     let x' = x - (vx / 2) in
     if x' < padding then padding
     else if x' + vx > arena_width + padding then arena_width + padding - vx
     else x')
    (let vy = V.y v |> int_of_float in
     let offset = 55 in
     let y' = y + offset in
     if y' + vy > arena_width + padding - offset then
       arena_width + padding - vy - offset
     else y')
    fontsize Color.gray

let draw_game cycles =
  let open Robot in
  draw_arena ();

  Array.iter draw_trail sprites;

  Array.iteri
    (fun i (r : Robot.t) ->
      update_sprite sprites.(i) r;
      if r.status = DEAD then draw_sprite sprites.(i) r;
      draw_stats i r sprites.(i).color)
    !all_robots;

  (* draw active robots on top of dead ones *)
  Array.iteri
    (fun i (r : Robot.t) -> if r.status = ALIVE then draw_sprite sprites.(i) r)
    !all_robots;

  draw_fps (Raylib.get_fps ());
  draw_cycles cycles
