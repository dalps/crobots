open Math
open CCFloat

let _robot_size = 50.
let _max_x = 1000.
let _max_y = 1000.
let _scan_duration = 5
let _trig_scale = 100_000.
let _res_limit = 10.

type status = ALIVE | DEAD

type t = {
  id : int;
  mutable status : status;
  mutable name : string;
  p : vector;
  dp : vector;
  mutable speed : float;
  mutable d_speed : float;
  mutable acceleration : float;
  mutable heading : float;
  mutable d_heading : float;
  mutable turret_heading : float;
  mutable d_turret_heading : float;
  mutable scan_degrees : float;
  mutable scan_cycles : int;
  mutable scan_res : float;
  mutable damage : float;
  mutable reload : int;
  mutable missiles : Missile.t array;
  mutable program : Ast.program;
  mutable ep : Ast.expression;
  mutable env : Memory.env_stack;
  mutable mem : Memory.memory;
}

let init id name program x y =
  {
    id;
    status = ALIVE;
    name;
    p = { x; y };
    dp = zero ();
    speed = 0.;
    d_speed = 0.;
    acceleration = 0.;
    heading = 0.;
    d_heading = 0.;
    turret_heading = 0.;
    d_turret_heading = 0.;
    scan_degrees = 0.;
    scan_cycles = 0;
    damage = 0.;
    scan_res = 0.;
    reload = 0;
    missiles = Array.init 2 (fun _ -> Missile.init ());
    program;
    ep = Ast.entry_point;
    env = Memory.init_stack ();
    mem = Memory.init_memory ();
  }

let cur_robot = ref (init 0 "foo" EMPTY 0. 0.)
let all_robots = ref [||]

let scan degree resolution =
  let degree, resolution = (of_int degree, of_int resolution) in
  let res =
    if abs resolution > _res_limit then _res_limit else abs resolution
  in
  let degree = normalize_degrees degree in
  let close_dist = ref 0. in
  !cur_robot.scan_cycles <- _scan_duration;
  !cur_robot.scan_degrees <- degree;
  !cur_robot.scan_res <- res;
  Array.iter
    (fun r ->
      if Stdlib.(!cur_robot.id <> r.id && r.status <> DEAD) then
        let v1, v2 = (rayvec_of_vector !cur_robot.p, rayvec_of_vector r.p) in
        let d = V.angle v1 v2 * _rad2deg |> round |> normalize_degrees in
        let d1, d2 =
          (normalize_degrees (d - res), normalize_degrees (d + res))
        in

        if is_between d1 degree d2 then
          let distance = V.distance v1 v2 in
          if (0. < distance && distance < !close_dist) || !close_dist = 0. then
            close_dist := distance)
    !all_robots;
  !close_dist |> round |> to_int

let cannon degree range =
  let degree, range = (of_int degree, of_int range) in
  let range =
    if range > Missile._mis_range then Missile._mis_range else range
  in
  let degree = normalize_degrees degree in
  if range <= 0. then 1
  else if CCInt.( > ) !cur_robot.reload 0 then 0
  else
    try
      Array.iter
        (fun (m : Missile.t) ->
          if Stdlib.( = ) m.status AVAIL then (
            !cur_robot.d_turret_heading <- degree;
            !cur_robot.reload <- Missile._reload_cycles;

            m.status <- FLYING;
            m.o.x <- !cur_robot.p.x;
            m.o.y <- !cur_robot.p.y;
            m.p.x <- !cur_robot.p.x;
            m.p.y <- !cur_robot.p.y;
            m.heading <- !cur_robot.turret_heading;
            m.range <- range;
            m.count <- Missile._explosion_cycles;
            raise Exit))
        !cur_robot.missiles;
      0
    with Exit -> 1

let drive degree speed =
  let degree, speed = (of_int degree, of_int speed) in
  !cur_robot.d_heading <- normalize_degrees degree;
  !cur_robot.d_speed <- perc_of_int speed

let damage () = !cur_robot.damage |> round |> to_int

let speed () = !cur_robot.speed |> max 0. |> min 100. |> round |> to_int

let heading () = !cur_robot.heading |> round |> to_int |> fun x -> x mod 360

let loc_x () = !cur_robot.p.x |> round |> to_int

let loc_y () = !cur_robot.p.y |> round |> to_int

let rand = Random.int

let sqrt x = of_int x |> abs |> sqrt |> round |> to_int

let int_trig ?(fact = 1.) f x =
  of_int x |> normalize_degrees |> ( * ) _deg2rad |> f |> ( * ) fact |> round
  |> to_int

let sin = int_trig sin ~fact:_trig_scale

let cos = int_trig cos ~fact:_trig_scale

let tan = int_trig tan ~fact:_trig_scale

let atan x =
  x |> of_int
  |> ( * ) (1. / _trig_scale)
  |> atan |> ( * ) _rad2deg |> round |> to_int
