open Crobots
open Sprite
open Gui
open Draw
open Defs

let debug = false

let usage_msg = "crobots <robot-programs>"

let motion_cycles = 15 (* one motion update every motion_cycles CPU cycles *)
let update_cycles = 30 (* one screen update every motion_cycles CPU cycles *)

let movement = ref motion_cycles (* counter for motion_cycles *)
let display = ref update_cycles (* counter for update_cycles *)
let cycle = ref 0 (* counter for CPU cycles *)

type gamestate = Play | End of Robot.t option

module Cmd = struct
  let parse () =
    let input_files = ref [] in

    let speclist = [] in
    let anon_fun filename = input_files := !input_files @ [ filename ] in
    Arg.parse speclist anon_fun usage_msg;

    match !input_files with
    | [] ->
        Arg.usage speclist usage_msg;
        print_endline "missing program, goodbye";
        exit 0
    | l -> l
end

let read_file filename =
  let ic = open_in filename in
  let out = ref "" in
  (try
     while true do
       let line = input_line ic in
       out := !out ^ "\n" ^ line
     done
   with End_of_file -> close_in_noerr ic);
  !out

let rand_pos () =
  let lt2 x = if x < 2 then 1 else 0 in
  let margin = 200 in
  let pos =
    Array.init 4 (fun k ->
        ( Raylib.get_random_value margin
            ((Robot._max_x *. 0.5 |> Float.to_int) - margin)
          + ((Robot._max_x *. 0.5 |> Float.to_int) * (k mod 2)),
          Raylib.get_random_value margin
            ((Robot._max_y *. 0.5 |> Float.to_int) - margin)
          + ((Robot._max_y *. 0.5 |> Float.to_int) * lt2 k) ))
  in
  Array.sort (fun _ _ -> -1 + Random.int 3) pos;
  pos

let start_robot (r : Robot.t) =
  let mem = Memory.init_memory () in
  let env = Memory.init_stack () in
  Trace.trace_instr (env, mem) (Instr r.program) |> ignore;
  r.ep <- Ast.entry_point;
  r.mem <- mem;
  r.env <- env

let reset_robot (r : Robot.t) (init_x, init_y) =
  let x, y = (init_x |> float, init_y |> float) in
  start_robot r;
  r.status <- ALIVE;
  r.dp.x <- 0.;
  r.dp.y <- 0.;
  r.p.x <- x;
  r.p.y <- y;
  r.acceleration <- 0.;
  r.damage <- 0.;
  r.speed <- 0.;
  r.d_speed <- 0.;
  r.heading <- 0.;
  r.turret_heading <- 0.;
  r.d_heading <- 0.;
  r.scan_degrees <- 0.;
  r.scan_cycles <- 0;
  r.scan_res <- 0.;
  r.reload <- 0;
  r.missiles <- Array.init 2 (fun _ -> Missile.init ())

let play () =
  let open Robot in
  incr cycle;

  Array.iter
    (fun r ->
      try
        match r.status with
        | ALIVE ->
            cur_robot := r;
            r.ep <- Trace.trace1_expr (r.env, r.mem) r.ep;
            Memory.janitor r.env r.mem
        | DEAD -> ()
      with _ ->
        Printf.printf "%s had to be restarted\n" r.name;
        start_robot r)
    !all_robots;

  decr movement;
  if !movement <= 0 then (
    Motion.update_all_robots !all_robots;
    movement := motion_cycles)

let rec loop state =
  match (Raylib.window_should_close (), state) with
  | true, _ ->
      unload_fonts ();
      Raylib.close_window ()
  | false, End winner ->
      let open Robot in
      let result =
        Option.fold ~none:"It's a tie"
          ~some:(fun (r : Robot.t) -> Printf.sprintf "%s won the match" r.name)
          winner
      in

      (* update *)
      play ();
      flush stdout;

      (* draw *)
      decr display;
      if !display <= 0 then (
        let open Raylib in
        begin_drawing ();
        clear_background background_color;
        draw_game !cycle;
        draw_endgame result; (* change to robot option *)
        end_drawing ();
        display := update_cycles);

      (* start a new match on key press *)
      let state' =
        let open Raylib in
        if is_key_down Key.R then (
          let init_pos = rand_pos () in
          reset_sprites ();
          Array.iteri (fun i r -> reset_robot r init_pos.(i)) !all_robots;
          Play)
        else state
      in

      loop state'
  | false, Play ->
      let open Robot in
      let robots_left =
        Array.fold_left
          (fun n (r : Robot.t) -> n + if r.status = ALIVE then 1 else 0)
          0 !all_robots
      in

      (* update *)
      play ();
      flush stdout;

      (* draw *)
      decr display;
      if !display <= 0 then (
        let open Raylib in
        begin_drawing ();
        clear_background background_color;
        draw_game !cycle;
        end_drawing ();
        display := update_cycles);

      (* the match is over when:
         - there's zero or one active robot left and
         - all missiles of dead robots have exploded *)
      let state' =
        if
          (robots_left <= if debug then -1 else 1)
          && Array.for_all
               (fun r ->
                 Array.for_all
                   (fun (m : Missile.t) -> m.status = AVAIL || r.status = ALIVE)
                   r.missiles)
               !all_robots
        then
          (* declare a winner and change the game state *)
          let winner =
            Array.fold_left
              (fun acc r ->
                match r.status with
                | ALIVE ->
                    Printf.printf "%s won the match\n" r.name;
                    Some r
                | _ -> acc)
              None !all_robots
          in
          End winner
        else
          let open Raylib in
          if is_key_pressed Key.R then (
            let init_pos = rand_pos () in
            reset_sprites ();
            Array.iteri (fun i r -> reset_robot r init_pos.(i)) !all_robots;
            Play)
          else state
      in

      loop state'

let setup () =
  Printexc.record_backtrace true;
  Random.init (Unix.time () |> int_of_float);

  (* parse the command line and parse the supplied robot programs *)
  let filenames = Cmd.parse () in
  let programs =
    List.map read_file filenames
    |> List.map2
         (fun f p ->
           let ast = Main.parse p in
           Printf.printf "%s compiled with no errors\n" f;
           (f, ast))
         filenames
    (* if only one robot was supplied, duplicate it *)
    |>
    if debug then fun x -> x
    else function
      | [ x ] -> [ x; x ]
      | l -> l
  in

  (* initialize raylib *)
  let open Raylib in
  init_window window_width window_height "crobots";
  load_fonts ();
  load_textures ();
  set_target_fps 0;

  (* initialize the robot array *)
  let open Robot in
  let init_pos = rand_pos () in
  let robots =
    List.mapi
      (fun i (name, program) ->
        let init_x, init_y = init_pos.(i) in
        let r = init i name program (init_x |> float) (init_y |> float) in
        start_robot r;
        cur_robot := r;
        sprites.(i) <-
          Sprite.create (get_screen_x r.p.x) (get_screen_y r.p.y) colors.(i);
        r)
      programs
  in
  all_robots := Array.of_list robots

let _ =
  setup ();
  loop Play
