open Raylib
open Crobots
open Gui
open Particles

module PS = ParticleSystem

let ratio = (arena_width |> float) /. Robot._max_x

let robot_width = Robot._robot_size *. ratio
let tank_width = robot_width
let turret_width = tank_width *. 0.7
let missile_width = robot_width *. 0.15
let missile_height = missile_width *. 2.
let scan_height = 70.

let colors = Raylib.[| Color.blue; Color.brown; Color.darkgreen; Color.pink |]

let max_frame_speed = 60.
let max_trail_speed = 15.

let trail_height = 6.

type missile_t = { bullet : R.t; trail : PS.t; explosion : PS.t }

type t = {
  tank : R.t;
  turret : R.t;
  missiles : missile_t array;
  color : Color.t;
  mutable frame_counter : int;
  mutable current_frame : int;
  trail : PS.t;
}

let get_screen_x x =
  let robot_x = x in
  (ratio *. robot_x) +. (padding |> float)

let get_screen_y y =
  let robot_y = y in
  (-1. *. ratio *. robot_y) +. (arena_width |> float) +. (padding |> float)

let get_screen_degrees d = -.d +. 270.

let create init_x init_y color =
  {
    tank = R.create init_x init_y tank_width tank_width;
    turret =
      R.create init_x init_y turret_width
        (turret_width
        *. ((Texture.height !turret_texture |> float)
           /. (Texture.width !turret_texture |> float)));
    missiles =
      Array.init 2 (fun _ ->
          {
            bullet = R.create init_x init_y missile_width missile_height;
            trail =
              PS.init ~emission_rate:20 ~avg_speed:50. ~randomize_speed:0.05
                ~avg_radius:1. ~randomize_radius:3. ~randomize_rotation:15.
                ~grow:0.1 ~duration:1. ~init_alpha:0.5 ~spread:15. ();
            explosion =
              PS.init ~emission_rate:60 ~avg_speed:150. ~randomize_speed:50.
                ~init_alpha:1. ~avg_radius:5. ~randomize_radius:2. ~grow:0.
                ~duration:0.5 ~spread:360. ();
          });
    color;
    frame_counter = 0;
    current_frame = 0;
    trail =
      PS.init ~emission_rate:5 ~avg_speed:0. ~avg_radius:5. ~duration:999. ();
  }

let sprites =
  Array.make 4
    (create
       ((window_width |> float) /. 2.)
       ((window_height |> float) /. 2.)
       Raylib.Color.black)

let reset_sprites () = Array.iter (fun s -> PS.clear s.trail) sprites

let update_sprite (s : t) (r : Robot.t) =
  let fps, dt = (get_fps (), get_frame_time ()) in

  let frame_speed = max_frame_speed *. (r.speed /. 100.) |> int_of_float in
  let trail_speed = max_trail_speed *. (r.d_speed /. 100.) |> int_of_float in

  if frame_speed <> 0 && r.status <> DEAD then (
    s.frame_counter <- s.frame_counter + 1;
    if s.frame_counter > fps / frame_speed then (
      s.frame_counter <- 0;
      s.current_frame <- (s.current_frame + 1) mod 4));

  let x = get_screen_x r.p.x in
  let y = get_screen_y r.p.y in

  R.set_x s.tank x;
  R.set_x s.turret x;
  R.set_y s.tank y;
  R.set_y s.turret y;

  PS.emit ~rate:trail_speed s.trail (V.create x y)
    (get_screen_degrees r.heading)
    fps;
  PS.simulate s.trail dt;

  Array.iter2
    (fun (sprite : missile_t) (m : Missile.t) ->
      let x = get_screen_x m.p.x in
      let y = get_screen_y m.p.y in
      R.set_x sprite.bullet x;
      R.set_y sprite.bullet y;
      V.set_x sprite.trail.origin x;
      V.set_y sprite.trail.origin y;
      (match m.status with
      | FLYING ->
          PS.emit sprite.trail (V.create x y)
            (get_screen_degrees m.heading -. 90.)
            fps
      | EXPLODING -> PS.emit sprite.explosion (V.create x y) 0. fps
      | _ -> ());
      PS.simulate sprite.trail dt;
      PS.simulate sprite.explosion dt)
    s.missiles r.missiles

let draw_trail (s : t) =
  let srcrec_trail = get_srcrec !trail_texture in
  let origin = V.create (tank_width /. 2.) (tank_width /. 2.) in

  PS.draw_texture s.trail !trail_texture srcrec_trail (R.width s.tank)
    trail_height origin s.color

let draw_sprite (s : t) (r : Robot.t) =
  let x = get_screen_x r.p.x in
  let y = get_screen_y r.p.y in

  let srcrec_turret = get_srcrec !turret_texture in

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
            Color.black
      | _ -> ())
    r.missiles;

  (* draw the tank, the cannon and the turret *)
  draw_circle_v (V.create x y)
    (Motion._collision_radius *. ratio)
    (fade Color.lightgray 0.5);

  let tank_texture_width = (Texture.width !tank_texture |> float) /. 4. in
  let w =
    R.width s.tank
    *. ((Texture.width !tank_shadow_texture |> float) /. tank_texture_width)
  in
  draw_texture_pro !tank_shadow_texture
    (get_srcrec !tank_shadow_texture)
    R.(create (x s.tank) (y s.tank) w w)
    (V.create (w /. 2.) (w /. 2.))
    (get_screen_degrees r.heading)
    Color.black;

  let frame_rec =
    R.create
      ((s.current_frame |> float) *. tank_texture_width)
      0. tank_texture_width
      (Texture.height !tank_texture |> float)
  in

  draw_texture_pro !tank_texture frame_rec s.tank
    (V.create (tank_width /. 2.) (tank_width /. 2.))
    (get_screen_degrees r.heading)
    color_tank;

  let w =
    R.width s.turret
    *. ((Texture.width !turret_shadow_texture |> float)
       /. (Texture.width !turret_texture |> float))
  in
  let h =
    R.height s.turret
    *. ((Texture.height !turret_shadow_texture |> float)
       /. (Texture.height !turret_texture |> float))
  in
  draw_texture_pro !turret_shadow_texture
    (get_srcrec !turret_shadow_texture)
    R.(create (x s.tank) (y s.tank) w h)
    (V.create (w /. 2.) (w /. 2.))
    (get_screen_degrees r.turret_heading)
    Color.black;

  draw_texture_pro !turret_texture srcrec_turret s.turret
    (V.create (turret_width /. 2.) (turret_width /. 2.))
    (get_screen_degrees r.turret_heading)
    color_turret;

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
      PS.draw_rec s.explosion Color.black)
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
