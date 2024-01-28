open Raylib
open CCFloat

type vector = { mutable x : t; mutable y : t }

let zero () = { x = 0.; y = 0. }

let normalize_degrees x = mod_float (x + 360.) 360.

let is_between start mid fin =
  mid - start |> normalize_degrees <= (fin - start |> normalize_degrees)

let perc_of_int n = max 0. n |> min 100.

module V = Vector2

let ( <-- ) v_old v_new =
  V.(
    set_x v_old (x v_new);
    set_y v_old (y v_new))

let rayvec_of_vector v = V.(create v.x v.y)
let vector_of_rayvec v = V.{ x = x v; y = y v }

let distance v w = V.distance (rayvec_of_vector v) (rayvec_of_vector w)
let angle v w = V.angle (rayvec_of_vector v) (rayvec_of_vector w)

let _deg2rad = pi / 180.
let _rad2deg = 180. / pi

let square_diag x =
  let x = (22.5 * Float.sin (((4. * x) - 90.) * _deg2rad)) + 22.5 in
  1. /. Float.cos (_deg2rad *. x)

let square_vertices center side heading =
  Array.init 4 (fun i ->
      let i = of_int i in
      let r = side * 0.5 in
      let a = (heading + 45. + (i * 90.)) * _deg2rad in
      {
        x = center.x + (sqrt 2. * r * Float.cos a);
        y = center.y + (sqrt 2. * r * Float.sin a);
      })

let square_vertices_v center side heading =
  Array.map rayvec_of_vector
    (square_vertices (vector_of_rayvec center) side heading)

let check_inter_circles c1 r1 c2 r2 =
  let r = r1 + r2 in
  let dx, dy = (c1.x - c2.x, c1.y - c2.y) in
  let sx, sy = ((if dx < 0. then -1. else 1.), if dy < 0. then -1. else 1.) in
  if distance c1 c2 <= r then
    ( true,
      {
        x = c2.x + (sx * sqrt ((r ** 2.) - (dy ** 2.)));
        y = c2.y + (sy * sqrt ((r ** 2.) - (dx ** 2.)));
      } )
  else (false, zero ())

let check_inter_circles_v c1 r1 c2 r2 =
  let b, v =
    check_inter_circles (vector_of_rayvec c1) r1 (vector_of_rayvec c2) r2
  in
  (b, rayvec_of_vector v)

let rec_axes vertices =
  let open V in
  [| subtract vertices.(0) vertices.(1); subtract vertices.(1) vertices.(2) |]
  |> Array.map normalize

module CollisionInfo = struct
  type t = {
    mutable test : bool;
    mutable penetration : float;
    mutable axis : V.t;
  }

  let init () = { test = false; penetration = 9999.; axis = V.zero () }
end

let check_intersection_squares vertices1 vertices2 =
  let axes = Array.append (rec_axes vertices1) (rec_axes vertices2) in
  let info = CollisionInfo.init () in

  let result =
    (* check for no gaps in every axis *)
    Array.for_all
      (fun a ->
        (* project vertices on the axis *)
        let projs1, projs2 =
          ( Array.map (fun v -> V.dot_product v a) vertices1,
            Array.map (fun v -> V.dot_product v a) vertices2 )
        in
        let minA, maxA, minB, maxB =
          ( Option.get (CCArray.min compare projs1),
            Option.get (CCArray.max compare projs1),
            Option.get (CCArray.min compare projs2),
            Option.get (CCArray.max compare projs2) )
        in

        let pleft, pright = (maxA - minB, maxB - minA) in
        let pen = min pleft pright in
        let dir = if pleft < pright then -1. else 1. in

        if pen < abs info.penetration then (
          info.penetration <- dir * pen;
          info.axis <- a);

        minA <= maxB && minB <= maxA)
      axes
  in
  info.test <- result;
  info

(* Get the velocity after the collision of an object A with object B
   Call with A and B swapped to get B's resulting velocity *)
let collide mass_a pos_a vel_a mass_b pos_b vel_b =
  V.(
    let collision_line = subtract pos_a pos_b in
    subtract vel_a
      (scale collision_line
         (2. * mass_b / (mass_a + mass_b)
         * (dot_product (subtract vel_a vel_b) collision_line
           / (length collision_line ** 2.)))))
