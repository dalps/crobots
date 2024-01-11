open Raylib
open Crobots

module V = Vector2

module Particle = struct
  type t = {
    mutable position : V.t;
    velocity : V.t;
    mutable rotation : float;
    mutable radius : float; (* radius increases over time *)
    mutable alpha : float; (* alpha decreases over time down to 0 *)
  }

  let init ~position ~velocity ~rotation ~radius =
    { position; velocity; rotation; radius; alpha = 1. }
end

module ParticleSystem = struct
  type t = {
    particles : Particle.t Queue.t;
    emission_rate : int; (* new particles per second *)
    avg_speed : float;
    avg_radius : float;
    duration : float; (* duration in seconds *)
    origin : V.t;
    spread : float; (* variance to emission heading *)
    mutable timer : int;
  }

  let init ~emission_rate ~avg_speed ~avg_radius ~duration ~origin ~spread =
    {
      particles = Queue.create ();
      emission_rate;
      avg_speed;
      avg_radius;
      duration;
      origin;
      spread;
      timer = 0;
    }

  (* emission stage *)
  let emit (ps : t) origin heading =
    ps.timer <- ps.timer + 1;
    if ps.timer > 60 / ps.emission_rate then (
      ps.timer <- 0;
      let direction_var = ps.spread in
      let speed_var = 5. in
      let radius_var = 2. in
      let speed =
        CCFloat.random_range
          (ps.avg_speed -. speed_var)
          (ps.avg_speed +. speed_var)
        |> CCRandom.run
      in
      let direction =
        CCFloat.random_range (heading -. direction_var)
          (heading +. direction_var)
        |> CCRandom.run
      in
      let radius =
        CCFloat.random_range
          (ps.avg_radius -. radius_var)
          (ps.avg_radius +. radius_var)
        |> CCRandom.run
      in
      let rotation = direction in
      Queue.add
        (Particle.init ~position:origin
           ~velocity:
             (V.create
                (speed *. Float.cos (Robot.deg2rad *. direction))
                (speed *. Float.sin (Robot.deg2rad *. direction)))
           ~rotation ~radius)
        ps.particles)

  (* simulation stage *)
  let simulate (ps : t) =
    Queue.iter
      (fun (p : Particle.t) ->
        V.(
          set_x p.position (x p.position +. x p.velocity);
          set_y p.position (y p.position +. y p.velocity));
        p.alpha <- p.alpha -. (1. /. (60. *. ps.duration));
        p.radius <- p.radius +. 0.01)
      ps.particles;

    if Queue.is_empty ps.particles |> not then
      let p = Queue.peek ps.particles in
      if p.alpha = 0. then Queue.take ps.particles |> ignore
end
