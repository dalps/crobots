open Crobots
open Robot
open Math

let%test "atan" =
  let s = 10000 in
  let l =
    List.init 10000 (fun i ->
        ( atan ((i - (s / 2)) * 100_000),
          Float.atan (float_of_int (i - (s / 2))) *. _rad2deg ))
  in
  List.for_all (fun (v, e) -> v = (e |> Float.round |> int_of_float)) l

let%test "cos" =
  let l =
    List.init 10000 (fun i ->
        (cos i, Float.cos (float_of_int i |> ( *. ) _deg2rad) *. 100_000.))
  in
  List.for_all (fun (v, e) -> v = (e |> Float.round |> int_of_float)) l

let%test "sin" =
  let l =
    List.init 10000 (fun i ->
        (sin i, Float.sin (float_of_int i |> ( *. ) _deg2rad) *. 100_000.))
  in
  List.for_all (fun (v, e) -> v = (e |> Float.round |> int_of_float)) l

let%test "tan" =
  let l =
    List.init 89 (fun i ->
        (tan i, Float.tan (float_of_int i |> ( *. ) _deg2rad) *. 100_000.))
  in
  List.for_all (fun (v, e) -> v = (e |> Float.round |> int_of_float)) l

let%test "drive" =
  let r1 = Robot.init 1 "test1" EMPTY 500. 500. in
  cur_robot := r1;
  drive 335 25 |> ignore;
  CCFloat.(r1.d_heading = 335. && r1.d_speed = 25.)

let%test "scan-1" =
  let r1 = Robot.init 1 "test1" EMPTY 0. 0. in
  let r2 = Robot.init 2 "test2" EMPTY 500. 500. in
  let r3 = Robot.init 3 "test3" EMPTY 700. 600. in
  all_robots := [| r1; r2; r3 |];
  cur_robot := r1;
  let d = scan 45 10 in
  d = 707

let%test "scan-2" =
  let r1 = Robot.init 1 "test1" EMPTY 0. 0. in
  let r2 = Robot.init 2 "test2" EMPTY 501. 0. in
  let r3 = Robot.init 3 "test3" EMPTY 432. 0. in
  all_robots := [| r1; r2; r3 |];
  cur_robot := r1;
  let d1 = scan 0 2 in
  cur_robot := r2;
  let d2 = scan 170 10 in
  cur_robot := r3;
  let d3 = scan 4 6 in
  d1 = 432 && d2 = 501 - 432 && d3 = 501 - 432

let%test "scan-3" =
  let r1 = Robot.init 1 "test1" EMPTY 0. 0. in
  let r2 = Robot.init 2 "test2" EMPTY 501. 0. in
  let r3 = Robot.init 3 "test3" EMPTY 50. 100. in
  all_robots := [| r1; r2; r3 |];
  cur_robot := r1;
  let d = scan 5 5 in
  d = 501

let%test "scan-3-1" =
  let r1 = Robot.init 1 "test1" EMPTY 0. 0. in
  let r2 = Robot.init 2 "test2" EMPTY 501. 0. in
  let r3 = Robot.init 3 "test3" EMPTY 50. 100. in
  all_robots := [| r1; r2; r3 |];
  cur_robot := r1;
  let d = scan 5 10 in
  d = 501

let%test "scan-3-2" =
  let r1 = Robot.init 1 "test1" EMPTY 0. 0. in
  let r2 = Robot.init 2 "test2" EMPTY 501. 10. in
  let r3 = Robot.init 3 "test3" EMPTY 50. 100. in
  all_robots := [| r1; r2; r3 |];
  cur_robot := r1;
  let d = scan 6 (-5) in
  d = 501

let%test "scan-4" =
  let r1 = Robot.init 1 "test1" EMPTY 0. 500. in
  let r2 = Robot.init 2 "test2" EMPTY 500. 0. in
  let r3 = Robot.init 3 "test3" EMPTY 50. 100. in
  all_robots := [| r1; r2; r3 |];
  cur_robot := r1;
  let d = scan (-45) 10 in
  d = 707

let%test "scan-5" =
  let r1 = Robot.init 1 "test1" EMPTY 0. 0. in
  let r2 = Robot.init 2 "test2" EMPTY 500. 0. in
  let r3 = Robot.init 3 "test3" EMPTY 50. 100. in
  all_robots := [| r1; r2; r3 |];
  cur_robot := r1;
  let d = scan 355 10 in
  d = 500

let%test "scan-out-of-reach-1" =
  let r1 = Robot.init 1 "test1" EMPTY 0. 0. in
  let r2 = Robot.init 2 "test2" EMPTY 501. 0. in
  let r3 = Robot.init 3 "test3" EMPTY 50. 100. in
  all_robots := [| r1; r2; r3 |];
  cur_robot := r1;
  let d = scan 5 4 in
  d = 0

let%test "scan-out-of-reach-2" =
  let r1 = Robot.init 1 "test1" EMPTY 0. 0. in
  let r2 = Robot.init 2 "test2" EMPTY 500. 0. in
  let r3 = Robot.init 3 "test3" EMPTY 700. 600. in
  all_robots := [| r1; r2; r3 |];
  cur_robot := r1;
  let d = scan 45 0 in
  d = 0

let%test "heading" =
  let r1 = Robot.init 1 "test1" EMPTY 500. 500. in
  all_robots := [| r1 |];
  cur_robot := r1;
  let d = heading () in
  drive 180 0;
  d = 0
