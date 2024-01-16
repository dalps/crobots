open Robot

let collision_slack = 50.

let square_diag x =
  let p = 45 in
  let x = if x / p mod 2 = 0 then x mod p else -(x mod p) + p in
  1. /. Float.cos (deg2rad *. (x |> float))

let collision_limit1 h =
  let w = robot_width * click |> float in
  square_diag h *. w /. 2.

let collision_limit2 h1 h2 =
  let w = robot_width * click |> float in
  (square_diag h1 *. w /. 2.) +. (square_diag h2 *. w /. 2.)

let update_robot i (r : t) =
  if r.reload > 0 then r.reload <- r.reload - 1;

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
    let dt = Raylib.get_frame_time () in
    let dr = (robot_speed |> float) *. dt in
    let rel_dr = (r.speed |> float) /. (click |> float) *. dr |> Int.of_float in
    r.range <- r.range + rel_dr;
    r.x <- r.org_x + (cos r.heading * (r.range / click) / 10_000);
    r.y <- r.org_y + (sin r.heading * (r.range / click) / 10_000);

    (* check for collision into another robot *)
    Array.iteri
      (fun i' r' ->
        if
          r'.status <> DEAD && i' <> i
          &&
          let x1, y1, x2, y2 = (float r.x, float r.y, float r'.x, float r'.y) in
          let distance =
            Float.sqrt (((x1 -. x2) ** 2.) +. ((y1 -. y2) ** 2.))
          in
          let limit = collision_limit2 r.heading r'.heading in
          limit -. collision_slack < distance && distance < limit
        then (
          r.speed <- 0;
          r.d_speed <- 0;
          r.damage <- min 100 (r.damage + collision);
          r'.speed <- 0;
          r'.d_speed <- 0;
          r'.damage <- min 100 (r'.damage + collision);
          Printf.printf "%s collided with %s (D%%: %d, D%%: %d)\n" r.name
            r'.name r.damage r'.damage;

          if r'.damage >= 100 then (
            Printf.printf "%s was killed by collision\n" r'.name;
            r'.damage <- 100;
            r'.status <- DEAD)))
      !all_robots;

    let limit = collision_limit1 r.heading |> int_of_float in
    (* check for collision into a wall *)
    (match r.x with
    | x when x < limit ->
        r.x <- limit;
        r.speed <- 0;
        r.d_speed <- 0;
        r.damage <- min 100 (r.damage + collision);
        Printf.printf "%s hit west wall (D%%: %d)\n" r.name r.damage
    | x when x > (max_x * click) - limit ->
        r.x <- (max_x * click) - limit - 1;
        r.speed <- 0;
        r.d_speed <- 0;
        r.damage <- min 100 (r.damage + collision);
        Printf.printf "%s hit east wall (D%%: %d)\n" r.name r.damage
    | _ -> ());

    (match r.y with
    | y when y < limit ->
        r.y <- limit;
        r.speed <- 0;
        r.d_speed <- 0;
        r.damage <- min 100 (r.damage + collision);
        Printf.printf "%s hit south wall (D%%: %d)\n" r.name r.damage
    | y when y > (max_y * click) - limit ->
        r.y <- (max_y * click) - limit - 1;
        r.speed <- 0;
        r.d_speed <- 0;
        r.damage <- min 100 (r.damage + collision);
        Printf.printf "%s hit north wall (D%%: %d)\n" r.name r.damage
    | _ -> ());

    if r.damage >= 100 then (
      Printf.printf "%s was killed by collision\n" r.name;
      r.damage <- 100;
      r.status <- DEAD))

let exp_damage dist =
  dist |> float_of_int |> fun x ->
  (if x < 20. then (-1. /. 3. *. x) +. (35. /. 3.)
   else max 0. ((-1. /. 10. *. x) +. 7.))
  |> Float.round |> int_of_float

let update_missiles (r : t) =
  Array.iter
    (fun (m : Missile.t) ->
      match m.status with
      | FLYING ->
          let x = ref 0 in
          let y = ref 0 in

          (* update missile position *)
          m.travelled <- m.travelled + Missile.mis_speed;
          if m.travelled > m.range then m.travelled <- m.range;
          m.cur_x <- m.beg_x + (cos m.heading * (m.travelled / click) / 10_000);
          x := m.cur_x;
          m.cur_y <- m.beg_y + (sin m.heading * (m.travelled / click) / 10_000);
          y := m.cur_y;

          (* check for missiles hitting walls *)
          if !x < 0 then (
            m.status <- EXPLODING;
            x := 1);
          if !x >= max_x * click then (
            m.status <- EXPLODING;
            x := (max_x * click) - 1);
          if !y < 0 then (
            m.status <- EXPLODING;
            y := 1);
          if !y >= max_y * click then (
            m.status <- EXPLODING;
            y := (max_y * click) - 1);

          (* missile reached target range, explode *)
          if m.travelled = m.range then m.status <- EXPLODING;

          (* inflict explosion damage on other robots *)
          if m.status = EXPLODING then
            Array.iter
              (fun (r' : t) ->
                if r'.status <> DEAD then (
                  x := (r'.x - m.cur_x) / click;
                  y := (r'.y - m.cur_y) / click;
                  let dist = sqrt ((!x * !x) + (!y * !y)) in
                  let dmg = exp_damage dist in
                  r'.damage <- min 100 (r'.damage + dmg);
                  if dmg <> 0 then
                    Printf.printf "%s was hit by %s's missile (D%%: %d)\n"
                      r'.name r.name r'.damage;
                  if r'.damage >= 100 then (
                    Printf.printf "%s was killed by %s's missile\n" r'.name
                      r.name;
                    r'.damage <- 100;
                    r'.status <- DEAD)))
              !all_robots
      | EXPLODING ->
          if m.count <= 0 then m.status <- AVAIL else m.count <- m.count - 1
      | _ -> ())
    r.missiles

let update_all_robots =
  Array.iteri (fun i r ->
      update_missiles r;
      if r.scan_cycles > 0 then r.scan_cycles <- r.scan_cycles - 1
      else r.scan_cycles <- 0;
      match (r.status, r.damage) with
      | DEAD, _ -> ()
      | ALIVE, d when d >= 100 ->
          r.damage <- 100;
          r.status <- DEAD
      | _ -> update_robot i r)
