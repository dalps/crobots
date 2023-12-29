open Crobots
open Robot

let%test "atan" =
  let s = 10000 in
  let l =
    List.init 10000 (fun i ->
        ( atan ((i - (s / 2)) * 100_000),
          Float.atan (float_of_int (i - (s / 2))) *. rad2deg ))
  in
  List.for_all (fun (v, e) -> v = (e |> Float.round |> int_of_float)) l

let%test "cos" =
  let l =
    List.init 10000 (fun i ->
        (cos i, Float.cos (float_of_int i |> ( *. ) deg2rad) *. 100_000.))
  in
  List.for_all (fun (v, e) -> v = (e |> Float.round |> int_of_float)) l

let%test "sin" =
  let l =
    List.init 10000 (fun i ->
        (sin i, Float.sin (float_of_int i |> ( *. ) deg2rad) *. 100_000.))
  in
  List.for_all (fun (v, e) -> v = (e |> Float.round |> int_of_float)) l

let%test "tan" =
  let l =
    List.init 89 (fun i ->
        (tan i, Float.tan (float_of_int i |> ( *. ) deg2rad) *. 100_000.))
  in
  List.for_all (fun (v, e) -> v = (e |> Float.round |> int_of_float)) l
