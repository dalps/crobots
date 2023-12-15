let scan = ( + )
let cannon = ( + )
let drive _ _ = ()
let damage () = 0
let speed () = 0
let loc_x () = 0
let loc_y () = 0

let rand = Random.int
let int_trig ?(fact = 1) f x =
  x mod 360 |> float_of_int
  |> (fun x -> x *. Float.pi /. 180.)
  |> f
  |> ( *. ) (float_of_int fact)
  |> Float.round
  |> int_of_float

let sqrt = int_trig Float.sqrt
let sin = int_trig Float.sin ~fact:100_000
let cos = int_trig Float.cos ~fact:100_000
let tan = int_trig Float.tan ~fact:100_000
let atan = int_trig Float.atan
