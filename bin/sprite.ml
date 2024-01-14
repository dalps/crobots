open Raylib
open Crobots
open Gui
open Particles

module PS = ParticleSystem

let robot_width = Robot.robot_width |> float
let tank_width = robot_width
let turret_width = tank_width *. 0.7
let cannon_width = tank_width *. 0.2
let cannon_height = tank_width *. 1.2
let missile_width = cannon_width
let missile_height = missile_width *. 1.5
let scan_height = 70.

let colors = Raylib.[| Color.blue; Color.brown; Color.darkgreen; Color.pink |]

let max_frame_speed = 60

let trail_time = 60
let max_trails = 50
let trail_height = 6.
let max_trail_speed = 75
let trail_fade = 0.0

let explosion_radius = 10.
let explosion_grow = 0.5
let explosion_speed = 0.1
let explosion_rate = 60
let explosion_duration = 2.
let explosion_spread = 180.
let explosion_alpha = 0.25

let smoke_duration = 1.
let smoke_frequency = 800

type trail = { x : float; y : float; rotation : float; mutable alpha : float }

type missile_t = { bullet : R.t; trail : PS.t }

type t = {
  tank : R.t;
  turret : R.t;
  missiles : missile_t array;
  color : Color.t;
  mutable frame_counter : int;
  mutable current_frame : int;
  mutable trail_frame_counter : int;
  trails : trail Queue.t;
}

let explosions =
  PS.init ~emission_rate:explosion_rate ~avg_speed:explosion_speed
    ~init_alpha:explosion_alpha ~avg_radius:explosion_radius
    ~grow:explosion_grow ~duration:explosion_duration ~spread:explosion_spread
    ()

(* flip coordinates on the x axis, scale down by 1/click and translate on the y axis *)
let get_screen_x x = (x / Robot.click) + padding
let get_screen_x_f x = get_screen_x x |> float
let get_screen_y y = (-1 * y / Robot.click) + arena_width + padding
let get_screen_y_f y = get_screen_y y |> float
let get_screen_degrees d = -d + 270
let get_screen_degrees_f d = get_screen_degrees d |> float

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
              PS.init ~emission_rate:smoke_frequency ~avg_speed:0.
                ~randomize_speed:0.5 ~avg_radius:5. ~grow:0.25
                ~duration:smoke_duration ~init_alpha:0.5 ~spread:45. ();
          });
    color;
    frame_counter = 0;
    current_frame = 0;
    trail_frame_counter = 0;
    trails = Queue.create ();
  }

let sprites =
  Array.make 4
    (create
       ((window_width |> float) /. 2.)
       ((window_height |> float) /. 2.)
       Raylib.Color.black)

let reset_sprites () = Array.iter (fun s -> Queue.clear s.trails) sprites

let update_sprite (s : t) (r : Robot.t) =
  let frame_speed =
    (max_frame_speed |> float) *. ((r.speed |> float) /. 100.) |> int_of_float
  in

  if frame_speed <> 0 && r.status <> DEAD then (
    s.frame_counter <- s.frame_counter + 1;
    if s.frame_counter > 60 / frame_speed then (
      s.frame_counter <- 0;
      s.current_frame <- (s.current_frame + 1) mod 4));

  let x = get_screen_x_f r.x in
  let y = get_screen_y_f r.y in

  let trail_frame_speed =
    (max_trail_speed |> float) *. ((r.speed |> float) /. 100.) |> int_of_float
  in

  (* is the tank moving? generate a new trail every 1/r.speed seconds *)
  if trail_frame_speed <> 0 && r.status <> DEAD then (
    s.trail_frame_counter <- s.trail_frame_counter + 1;
    if s.trail_frame_counter > 60 / trail_frame_speed then (
      s.trail_frame_counter <- 0;
      Queue.add
        { x; y; rotation = get_screen_degrees_f r.heading; alpha = 1.0 }
        s.trails));

  Queue.iter (fun (t : trail) -> t.alpha <- t.alpha -. trail_fade) s.trails;

  (if Queue.is_empty s.trails |> not then
     let q = Queue.peek s.trails in
     if q.alpha = 0. then Queue.take s.trails |> ignore);

  R.set_x s.tank x;
  R.set_x s.turret x;
  R.set_y s.tank y;
  R.set_y s.turret y;
  Array.iter2
    (fun (sprite : missile_t) (m : Missile.t) ->
      let x = get_screen_x_f m.cur_x in
      let y = get_screen_y_f m.cur_y in
      R.set_x sprite.bullet x;
      R.set_y sprite.bullet y;
      V.set_x sprite.trail.origin x;
      V.set_y sprite.trail.origin y;
      (match m.status with
      | FLYING ->
          PS.emit sprite.trail (V.create x y)
            (get_screen_degrees_f m.heading -. 90.)
      | EXPLODING -> PS.emit explosions (V.create x y) 0.
      | _ -> ());
      PS.simulate sprite.trail;
      PS.simulate explosions)
    s.missiles r.missiles

let draw_trail (s : t) =
  let srcrec_trail = get_srcrec !trail_texture in

  Queue.iter
    (fun (tr : trail) ->
      draw_texture_pro !trail_texture srcrec_trail
        R.(create tr.x tr.y (width s.tank) trail_height)
        (V.create (tank_width /. 2.) (tank_width /. 2.))
        tr.rotation s.color)
    s.trails

let draw_sprite (s : t) (r : Robot.t) =
  let x = get_screen_x r.x in
  let y = get_screen_y r.y in

  let module R = R in
  let srcrec_turret = get_srcrec !turret_texture in

  let color_tank, color_turret =
    match r.status with
    | ALIVE ->
        (* draw the scan radar *)
        let res = r.scan_res |> float in
        let dir = r.scan_degrees + 90 |> float in
        if r.scan_cycles > 0 then (
          draw_circle_sector
            (V.create (x |> float) (y |> float))
            scan_height (dir +. res) (dir -. res) 1 (fade Color.red 0.1);
          draw_circle_lines x y scan_height (fade Color.red 0.1));
        (s.color, s.color)
    | DEAD ->
        let g1 = color_brightness Color.gray 0.5 in
        let g2 = color_brightness Color.gray 0.7 in
        (g1, g2)
  in

  let tank_texture_width = (Texture.width !tank_texture |> float) /. 4. in

  (* draw the tank, the cannon and the turret *)
  let w =
    R.width s.tank
    *. ((Texture.width !tank_shadow_texture |> float) /. tank_texture_width)
  in
  draw_texture_pro !tank_shadow_texture
    (get_srcrec !tank_shadow_texture)
    R.(create (x s.tank) (y s.tank) w w)
    (V.create (w /. 2.) (w /. 2.))
    (get_screen_degrees_f r.heading)
    Color.black;

  let frame_rec =
    R.create
      ((s.current_frame |> float) *. tank_texture_width)
      0. tank_texture_width
      (Texture.height !tank_texture |> float)
  in

  draw_texture_pro !tank_texture frame_rec s.tank
    (V.create (tank_width /. 2.) (tank_width /. 2.))
    (get_screen_degrees_f r.heading)
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
    (get_screen_degrees_f r.turret_heading)
    Color.black;

  draw_texture_pro !turret_texture srcrec_turret s.turret
    (V.create (turret_width /. 2.) (turret_width /. 2.))
    (get_screen_degrees_f r.turret_heading)
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
  Array.iteri
    (fun i (m : Missile.t) ->
      Queue.iter
        (fun (p : Particle.t) ->
          let particle_width = p.radius in
          let particle_rec =
            R.create (V.x p.position) (V.y p.position) particle_width
              particle_width
          in
          draw_rectangle_pro particle_rec
            (V.create (particle_width /. 2.) (particle_width /. 2.))
            p.rotation (fade Color.gray p.alpha))
        s.missiles.(i).trail.particles;

      match m.status with
      | FLYING ->
          draw_rectangle_pro s.missiles.(i).bullet
            (V.create (missile_width /. 2.) (missile_height /. 2.))
            (get_screen_degrees_f m.heading)
            Color.black
      | _ -> ())
    r.missiles;

  Queue.iter
    (fun (p : Particle.t) ->
      draw_circle_v p.position p.radius (fade Color.gray p.alpha))
    explosions.particles;

  (* draw the robot's name floating around its sprite *)
  let fontsize = 20. in
  let v = measure_text_ex !stat_font r.name fontsize font_spacing in
  draw_stat_text_s r.name
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
