open Math
open Robot
open CCFloat
open Raylib

let _angular_velocity = 360.
let _friction = 5.
let _collision_radius = _robot_size * 0.5 * Float.sqrt 2.
let _collision_damage = 0.05
let _rebound = -1.

let update_robot (r : Robot.t) dt =
  (* update speed, moderated by acceleration *)
  r.speed <- V.length (rayvec_of_vector r.dp);
  r.acceleration <- max 0. (r.acceleration + (r.d_speed - r.speed));

  (* update heading *)
  let dr = r.d_heading - r.heading in
  let s =
    if equal_precision ~epsilon:0.001 (abs dr) 180. then 1.
    else Float.sin (dr * _deg2rad)
  in
  let angular_velocity = _angular_velocity * s in
  r.heading <- r.heading + (angular_velocity * dt) |> normalize_degrees;

  (* update turret heading *)
  let dr = r.d_turret_heading - r.turret_heading in
  let s =
    if equal_precision ~epsilon:0.001 (abs dr) 180. then 1.
    else Float.sin (dr * _deg2rad)
  in
  let angular_velocity = _angular_velocity * s in
  r.turret_heading <-
    r.turret_heading + (angular_velocity * dt) |> normalize_degrees;

  let ddp =
    V.(
      create
        (r.acceleration * Float.cos (_deg2rad * r.heading))
        (r.acceleration * Float.sin (_deg2rad * r.heading))
      |> (fun v -> rotate v (angular_velocity * dt * _deg2rad))
      |> vector_of_rayvec)
  in

  ddp.x <- ddp.x - (r.dp.x * _friction);
  ddp.y <- ddp.y - (r.dp.y * _friction);

  r.dp.x <- r.dp.x + (ddp.x * dt);
  r.dp.y <- r.dp.y + (ddp.y * dt);

  r.p.x <- r.p.x + (r.dp.x * dt) + (ddp.x * dt * dt * 0.5);
  r.p.y <- r.p.y + (r.dp.y * dt) + (ddp.y * dt * dt * 0.5);

  (* check for collision into another robot *)
  let colliding =
    Array.exists
      (fun (r' : Robot.t) ->
        let colliding, offset =
          check_inter_circles r.p _collision_radius r'.p _collision_radius
        in
        let _, offset' =
          check_inter_circles r'.p _collision_radius r.p _collision_radius
        in
        if Stdlib.(r.id <> r'.id && r'.status <> DEAD) && colliding then (
          r.p.x <- offset.x;
          r.p.y <- offset.y;
          r'.p.x <- offset'.x;
          r'.p.y <- offset'.y;
          r'.dp.x <- r'.dp.x * _rebound;
          r'.dp.y <- r'.dp.y * _rebound;
          r'.d_speed <- 0.;
          r'.damage <- r'.damage + (_collision_damage / 100. * r'.speed);
          true)
        else false)
      !all_robots
  in

  (* check for collision into a wall *)
  let east, north, west, south =
    ( r.p.x < _collision_radius,
      r.p.y < _collision_radius,
      r.p.x > _max_y - _collision_radius,
      r.p.y > _max_y - _collision_radius )
  in
  if east then r.p.x <- _collision_radius;
  if west then r.p.x <- _max_x - _collision_radius;
  if north then r.p.y <- _collision_radius;
  if south then r.p.y <- _max_y - _collision_radius;

  (* collision consequences *)
  if east || north || west || south || colliding then (
    r.damage <- r.damage + (_collision_damage / 100. * r.speed);
    r.dp.x <- r.dp.x * _rebound;
    r.dp.y <- r.dp.y * _rebound;
    r.d_speed <- 0.)

let exp_damage x =
  if x < 20. then (-1. /. 3. *. x) +. (35. /. 3.)
  else max 0. ((-1. /. 10. *. x) +. 7.)

let update_missiles (r : Robot.t) dt =
  Array.iter
    (fun (m : Missile.t) ->
      match m.status with
      | FLYING ->
          (* update missile position *)
          let dr = Missile._mis_speed *. dt in
          m.p.x <- m.p.x + (Float.cos (m.heading * _deg2rad) * dr);
          m.p.y <- m.p.y + (Float.sin (m.heading * _deg2rad) * dr);

          (* check for missiles hitting walls or reaching target range *)
          if
            m.p.x < 0. || m.p.x >= _max_x || m.p.y < 0. || m.p.y >= _max_y
            || distance m.p m.o >= m.range
          then m.status <- EXPLODING;

          (* inflict explosion damage on other robots *)
          if Stdlib.(m.status = EXPLODING) then
            Array.iter
              (fun (r' : Robot.t) ->
                if Stdlib.(r'.status <> DEAD) then (
                  let dmg = exp_damage (distance r'.p m.p) in
                  r'.damage <- min 100. (r'.damage + dmg);
                  if dmg >= 0. then
                    Printf.printf "%s was hit by %s's missile (D%%: %.0f)\n"
                      r'.name r.name r'.damage;
                  if r'.damage >= 100. then (
                    Printf.printf "%s was killed by %s's missile\n" r'.name
                      r.name;
                    r'.damage <- 100.;
                    r'.status <- DEAD)))
              !all_robots
      | EXPLODING ->
          Stdlib.(
            if m.count <= 0 then m.status <- AVAIL else m.count <- m.count - 1)
      | _ -> ())
    r.missiles

let update_all_robots =
  Array.iter (fun (r : Robot.t) ->
      update_missiles r (get_frame_time ());
      CCInt.(
        if r.scan_cycles > 0 then r.scan_cycles <- r.scan_cycles - 1
        else r.scan_cycles <- 0);
      match (r.status, r.damage) with
      | DEAD, _ -> ()
      | ALIVE, d when d >= 100. ->
          r.damage <- 100.;
          r.status <- DEAD
      | _ ->
          CCInt.(if r.reload > 0 then r.reload <- r.reload - 1);
          update_robot r (get_frame_time ()))
