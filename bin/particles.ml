open Raylib
open CCFloat
open Crobots.Math

module Particle = struct
  type t = {
    position : V.t;
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
    randomize_rotation : float;
    init_alpha : float;
    grow : float; (* radius increase / decrease per frame *)
    mutable timer : int;
  }

  let init ?(spread = 0.) ?(randomize_speed = 0.) ?(randomize_radius = 0.)
      ?(randomize_rotation = 0.) ?(grow = 0.) ?(origin = Vector2.zero ())
      ?(init_alpha = 1.) ~emission_rate ~avg_speed ~avg_radius ~duration () =
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
      randomize_rotation;
      init_alpha;
      grow;
      timer = 0;
    }

  (* emission stage *)
  let emit (ps : t) ?(rate = 0) origin heading fps =
    CCInt.(ps.timer <- ps.timer + 1);
    let rate =
      if CCInt.(rate <> 0) then rate else ps.emission_rate
    in
    let f = CCInt.(fps / rate) in
    if CCInt.(f <> 0 && ps.timer mod f = 0) then
      let speed =
        random_range
          (ps.avg_speed - ps.randomize_speed)
          (ps.avg_speed + ps.randomize_speed)
        |> CCRandom.run
      in
      let rotation =
        random_range
          (heading - ps.randomize_rotation)
          (heading + ps.randomize_rotation)
        |> CCRandom.run
      in
      let direction =
        random_range (heading - ps.spread) (heading + ps.spread) |> CCRandom.run
      in
      let radius =
        random_range
          (ps.avg_radius - ps.randomize_radius)
          (ps.avg_radius + ps.randomize_radius)
        |> CCRandom.run
      in
      Queue.add
        (Particle.init
           ~position:V.(create (x origin) (y origin))
           ~velocity:
             (V.create
                (speed * Float.cos (_deg2rad * direction))
                (speed * Float.sin (_deg2rad * direction)))
           ~rotation ~radius ~alpha:ps.init_alpha)
        ps.particles

  (* simulation stage *)
  let simulate (ps : t) dt =
    Queue.iter
      (fun (p : Particle.t) ->
        V.(
          set_x p.position (x p.position + (x p.velocity * dt));
          set_y p.position (y p.position + (y p.velocity * dt)));
        let da = (0. - ps.init_alpha) / ps.duration in
        let a = da * dt in
        p.alpha <- p.alpha + a;
        p.radius <- p.radius + ps.grow)
      ps.particles;

    if Queue.is_empty ps.particles |> not then
      let p = Queue.peek ps.particles in
      if p.alpha <= 0. then Queue.take ps.particles |> ignore

  let clear (ps : t) = Queue.clear ps.particles

  let draw_rec (ps : t) color =
    Queue.iter
      (fun (p : Particle.t) ->
        draw_rectangle_pro
          (Rectangle.create V.(x p.position) V.(y p.position) p.radius p.radius)
          (V.create (p.radius * 0.5) (p.radius * 0.5))
          p.rotation (fade color p.alpha))
      ps.particles

  let draw_circle (ps : t) color =
    Queue.iter
      (fun (p : Particle.t) ->
        draw_circle_v p.position p.radius (fade color p.alpha))
      ps.particles

  let draw_texture (ps : t) texture srcrec width height origin color =
    Queue.iter
      (fun (p : Particle.t) ->
        draw_texture_pro texture srcrec
          (Rectangle.create V.(x p.position) V.(y p.position) width height)
          origin p.rotation (fade color p.alpha))
      ps.particles
end
