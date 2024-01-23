open Raylib
open Crobots
open Defs

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
              PS.init ~emission_rate:30 ~avg_speed:50. ~randomize_speed:0.05
                ~avg_radius:3. ~randomize_radius:3. ~randomize_rotation:15.
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
