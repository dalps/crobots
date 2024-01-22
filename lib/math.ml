open Raylib
open CCFloat

type vector = { mutable x : t; mutable y : t }

let zero () = { x = 0.; y = 0. }

let normalize_degrees x = mod_float (x + 360.) 360.

let is_between start mid fin =
  mid - start |> normalize_degrees <= (fin - start |> normalize_degrees)

let perc_of_int n = max 0. n |> min 100.

module V = Vector2

let rayvec_of_vector v = V.(create v.x v.y)
let vector_of_rayvec v = V.{ x = x v; y = y v }

let distance v w = V.distance (rayvec_of_vector v) (rayvec_of_vector w)
let angle v w = V.angle (rayvec_of_vector v) (rayvec_of_vector w)

let _deg2rad = pi / 180.
let _rad2deg = 180. / pi

let _square_diag x =
  let x = (22.5 * Float.sin (((4. * x) - 90.) * _deg2rad)) + 22.5 in
  1. /. Float.cos (_deg2rad *. x)

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
