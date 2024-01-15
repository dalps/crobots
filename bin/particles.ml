open Raylib
open Crobots

module V = Vector2

module Particle = struct
  type t = {
    mutable position : V.t;
    velocity : V.t;
    mutable rotation : float;
    mutable radius : float; (* radius increases over time *)
    mutable alpha : float; (* alpha decreases over time down to 0. *)
  }

  let init ~position ~velocity ~rotation ~radius ~alpha =
    { position; velocity; rotation; radius; alpha }
end

module ParticleSystem = struct
  type t = {
    particles : Particle.t Queue.t;
    emission_rate : int; (* new particles per second *)
    avg_speed : float;
    avg_radius : float;
    duration : float; (* duration in seconds *)
    origin : V.t;
    spread : float; (* variance to emission heading (0. - 180.) *)
    randomize_speed : float;
    randomize_radius : float;
    init_alpha : float;
    grow : float; (* radius increase / decrease per frame *)
    mutable timer : int;
  }

  let init ?(spread = 0.) ?(randomize_speed = 0.) ?(randomize_radius = 0.)
      ?(grow = 0.) ?(origin = Vector2.zero ()) ?(init_alpha = 1.) ~emission_rate
      ~avg_speed ~avg_radius ~duration () =
    {
      particles = Queue.create ();
      emission_rate;
      avg_speed;
      avg_radius;
      duration;
      origin;
      spread;
      randomize_speed;
      randomize_radius;
      init_alpha;
      grow;
      timer = 0;
    }

  (* emission stage *)
  let emit (ps : t) origin heading =
    ps.timer <- ps.timer + 1;
    if ps.timer > 60 / ps.emission_rate then (
      ps.timer <- 0;
      let speed =
        CCFloat.random_range
          (ps.avg_speed -. ps.randomize_speed)
          (ps.avg_speed +. ps.randomize_speed)
        |> CCRandom.run
      in
      let direction =
        CCFloat.random_range (heading -. ps.spread) (heading +. ps.spread)
        |> CCRandom.run
      in
      let radius =
        CCFloat.random_range
          (ps.avg_radius -. ps.randomize_radius)
          (ps.avg_radius +. ps.randomize_radius)
        |> CCRandom.run
      in
      let rotation = 0. in
      Queue.add
        (Particle.init ~position:origin
           ~velocity:
             (V.create
                (speed *. Float.cos (Robot.deg2rad *. direction))
                (speed *. Float.sin (Robot.deg2rad *. direction)))
           ~rotation ~radius ~alpha:ps.init_alpha)
        ps.particles)

  (* simulation stage *)
  let simulate (ps : t) =
    Queue.iter
      (fun (p : Particle.t) ->
        V.(
          set_x p.position (x p.position +. x p.velocity);
          set_y p.position (y p.position +. y p.velocity));
        p.alpha <- p.alpha -. (ps.init_alpha /. (60. *. ps.duration));
        p.radius <- p.radius +. ps.grow)
      ps.particles;

    if Queue.is_empty ps.particles |> not then
      let p = Queue.peek ps.particles in
      if p.alpha <= 0. then Queue.take ps.particles |> ignore
end
