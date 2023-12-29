let robot_speed = 7
let turn_speed = 50
let accel = 10
let collision = 5

let click = 10
let max_x = 1000
let max_y = 1000

let scanning_duration = 5

let trig_scale = 100_000.
let res_limit = 10

let deg2rad = Float.pi /. 180.
let rad2deg = 180. /. Float.pi

type status = ALIVE | DEAD

type t = {
  mutable status : status;
  mutable name : string;
  mutable x : int;
  mutable y : int;
  mutable org_x : int;
  mutable org_y : int;
  mutable range : int;
  mutable last_x : int;
  mutable last_y : int;
  mutable damage : int;
  mutable last_damage : int;
  mutable speed : int;
  mutable last_speed : int;
  mutable d_speed : int;
  mutable accel : int;
  mutable heading : int;
  mutable last_heading : int;
  mutable d_heading : int;
  mutable scan_degrees : int;
  mutable reload : int;
  mutable program : Ast.program;
  mutable ep : Ast.expression;
  mutable env : Memory.env_stack;
  mutable mem : Memory.memory;
  mutable missiles : Missile.t array;
  mutable scanning_cycles : int;
}

let init () =
  {
    status = ALIVE;
    name = "foo";
    x = 0;
    y = 0;
    org_x = 0;
    org_y = 0;
    range = 0;
    last_x = 0;
    last_y = 0;
    damage = 0;
    last_damage = 0;
    speed = 0;
    last_speed = 0;
    d_speed = 0;
    accel = 0;
    heading = 0;
    last_heading = 0;
    d_heading = 0;
    scan_degrees = 0;
    reload = 0;
    missiles = Array.init 2 (fun _ -> Missile.init ());
    program = EMPTY;
    ep = CALL ("main", []);
    env = Memory.init_stack ();
    mem = Memory.init_memory ();
    scanning_cycles = 0;
  }

let cur_robot = ref (init ())
let all_robots = ref [||]

let degree_of_int d = abs d mod 360
let perc_of_int n = max 0 n |> min 100

let cannon degree range =
  let range = if range > Missile.mis_range then Missile.mis_range else range in
  let degree = degree_of_int degree in
  if range <= 0 then 1
  else if !cur_robot.reload > 0 then 0
  else
    try
      Array.iter
        (fun (m : Missile.t) ->
          if m.status = AVAIL then (
            !cur_robot.scan_degrees <- degree;
            !cur_robot.reload <- Missile.reload_cycles;
            m.status <- FLYING;
            m.beg_x <- !cur_robot.x;
            m.beg_y <- !cur_robot.y;
            m.cur_x <- !cur_robot.x;
            m.cur_y <- !cur_robot.y;
            m.heading <- degree;
            m.range <- range * click;
            m.travelled <- 0;
            m.count <- Missile.explosion_cycles;
            Printf.printf "%s fired a missile\n" !cur_robot.name;
            raise Exit))
        !cur_robot.missiles;
      0
    with Exit -> 1

let drive degree speed =
  !cur_robot.d_heading <- degree_of_int degree;
  !cur_robot.d_speed <- perc_of_int speed

let damage () = !cur_robot.damage

let speed () = !cur_robot.speed

let loc_x () = !cur_robot.x

let loc_y () = !cur_robot.y

let rand = Random.int

let sqrt x = abs x |> float_of_int |> Float.sqrt |> Float.round |> int_of_float

let int_trig ?(fact = 1.) f x =
  degree_of_int x |> float_of_int |> ( *. ) deg2rad |> f |> ( *. ) fact
  |> Float.round |> int_of_float

let sin = int_trig Float.sin ~fact:trig_scale

let cos = int_trig Float.cos ~fact:trig_scale

let tan = int_trig Float.tan ~fact:trig_scale

let atan x =
  x |> float_of_int
  |> ( *. ) (1. /. trig_scale)
  |> Float.atan |> ( *. ) rad2deg |> Float.round |> int_of_float

let scan degree resolution =
  let res = if abs resolution > res_limit then res_limit else resolution in
  let degree = degree_of_int degree in
  let close_dist = ref 0. in
  !cur_robot.scanning_cycles <- scanning_duration;
  !cur_robot.scan_degrees <- degree;
  try
    Array.iter
      (fun r ->
        if r = !cur_robot || r.status = DEAD then raise Exit;
        let x = (!cur_robot.x / click) - (r.x / click) |> float_of_int in
        let y = (!cur_robot.y / click) - (r.y / click) |> float_of_int in
        let d = ref 0 in

        if x <> 0. then
          let d' = Float.atan (y /. x) *. rad2deg |> int_of_float in
          match (r.x >= !cur_robot.x, r.y >= !cur_robot.y) with
          | true, true -> d := d' (* 1st quadrant *)
          | true, false -> d := 360 + d' (* 4th quadrant *)
          | _ -> d := 180 + d' (* 2nd - 3rd quadrant *)
        else d := if r.y > !cur_robot.y then 90 else 270;

        let b = if degree > res && degree < 360 - res then 0 else 180 in
        let dd = b + degree in
        let d1 = b + !d - res in
        let d2 = b + !d + res in

        if dd >= d1 && dd <= d2 then
          let distance = Float.sqrt ((x *. x) +. (y *. y)) in
          if distance < !close_dist || !close_dist = 0. then
            close_dist := distance)
      !all_robots;
    !close_dist |> int_of_float
  with Exit -> !close_dist |> int_of_float

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
    r.range <- r.range + (r.speed / click * robot_speed);
    r.x <- r.org_x + (cos r.heading * (r.range / click) / 100000);
    r.y <- r.org_y + (sin r.heading * (r.range / click) / 100000);

    (* check for collision into another robot *)
    Array.iteri
      (fun i' r' ->
        if
          r'.status <> DEAD && i' <> i
          && abs (r.x - r'.x) < click
          && abs (r.y - r'.y) < click
        then (
          Printf.printf "%d:%s collided with %d:%s\n" i r.name i' r'.name;
          r.speed <- 0;
          r.d_speed <- 0;
          r.damage <- r.damage + collision;
          r'.speed <- 0;
          r'.d_speed <- 0;
          r'.damage <- r'.damage + collision))
      !all_robots;

    (* check for collisions with a wall *)
    (match r.x with
    | x when x < 0 ->
        Printf.printf "%d:%s hit west wall\n" i r.name;
        r.x <- 0;
        r.speed <- 0;
        r.d_speed <- 0;
        r.damage <- r.damage + collision
    | x when x > max_x * click ->
        Printf.printf "%d:%s hit east wall\n" i r.name;
        r.x <- (max_x * click) - 1;
        r.speed <- 0;
        r.d_speed <- 0;
        r.damage <- r.damage + collision
    | _ -> ());

    match r.y with
    | y when y < 0 ->
        Printf.printf "%d:%s hit south wall\n" i r.name;
        r.y <- 0;
        r.speed <- 0;
        r.d_speed <- 0;
        r.damage <- r.damage + collision
    | y when y > max_y * click ->
        Printf.printf "%d:%s hit north wall\n" i r.name;
        r.y <- (max_y * click) - 1;
        r.speed <- 0;
        r.d_speed <- 0;
        r.damage <- r.damage + collision
    | _ -> ())

let exp_damage dist =
  dist |> float_of_int |> fun x ->
  max 0 ((x *. (-1. /. 3.)) +. (35. /. 3.) |> int_of_float)

let update_missiles (r : t) =
  Array.iter
    (fun (m : Missile.t) ->
      if m.count <= 0 then m.status <- AVAIL else m.count <- m.count - 1;
      match m.status with
      | FLYING ->
          let x = ref 0 in
          let y = ref 0 in
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

          if m.travelled = m.range then m.status <- EXPLODING;

          if m.status = EXPLODING then
            Array.iter
              (fun (r : t) ->
                if r.status <> DEAD then (
                  x := (r.x - m.cur_x) / click;
                  y := (r.x - m.cur_x) / click;
                  let dist = sqrt ((!x * !x) + (!y * !y)) in
                  r.damage <- r.damage + exp_damage dist;
                  if r.damage >= 100 then (
                    r.damage <- 100;
                    r.status <- DEAD)))
              !all_robots
      | EXPLODING -> ()
      | _ -> ())
    r.missiles

let update_all_robots =
  Array.iteri (fun i r ->
      update_missiles r;
      if r.scanning_cycles > 0 then r.scanning_cycles <- r.scanning_cycles - 1
      else r.scanning_cycles <- 0;
      match (r.status, r.damage) with
      | DEAD, _ -> ()
      | ALIVE, d when d >= 100 ->
          r.damage <- 100;
          r.status <- DEAD
      | _ -> update_robot i r)
