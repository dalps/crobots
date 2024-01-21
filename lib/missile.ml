open Math

let _mis_speed = 500.
let _mis_range = 700.
let _reload_cycles = 15
let _explosion_cycles = 5

type status = AVAIL | FLYING | EXPLODING

let string_of_status = function
  | AVAIL -> "avl"
  | FLYING -> "fly"
  | EXPLODING -> "exp"

type t = {
  mutable status : status;
  p : vector;
  o : vector;
  mutable heading : float;
  mutable range : float;
  mutable count : int;
}

let init () =
  {
    status = AVAIL;
    p = zero ();
    o = zero ();
    heading = 0.;
    range = 0.;
    count = 0;
  }
