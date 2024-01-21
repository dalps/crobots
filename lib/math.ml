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

let _deg2rad = pi / 180.
let _rad2deg = 180. / pi
