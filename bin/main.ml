open Crobots
open Sprite
open Gui

let usage_msg = "crobots <robot-programs>"

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
   with _ -> close_in_noerr ic);
  !out

let motion_cycles = 15
let update_cycles = 30
let c = ref 0
let movement = ref motion_cycles
let display = ref update_cycles
let ( += ) r n = r := !r + n

let draw_game () =
  let open Raylib in
  draw_arena ();

  Array.iteri
    (fun i (r : Robot.t) ->
      let open Sprite in
      update_sprite sprites.(i) r;
      draw_sprite sprites.(i) r;
      draw_stats i r sprites.(i).color)
    Robot.(!all_robots);

  Gui.draw_fps (get_fps ());
  c += update_cycles;
  draw_cycles !c

let rec loop () =
  let open Robot in
  let robots_left =
    Array.fold_left
      (fun n r -> n + if r.status = ALIVE then 1 else 0)
      0 !all_robots
  in
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false when robots_left <= 1 -> ()
  | false ->
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
            r.ep <- CALL ("main", []);
            r.env <- Memory.init_stack ();
            r.mem <- Memory.init_memory ())
        !all_robots;

      decr movement;
      if !movement <= 0 then (
        Motion.update_all_robots !all_robots;
        movement := motion_cycles);

      decr display;
      if !display <= 0 then (
        let open Raylib in
        begin_drawing ();
        clear_background background_color;
        draw_game ();
        end_drawing ();
        display := update_cycles);

      flush stdout;
      loop ()

let rec endgame result =
  let open Robot in
  match Raylib.window_should_close () with
  | true ->
      unload_fonts ();
      Raylib.close_window ()
  | false ->
      decr movement;
      if !movement <= 0 then (
        Motion.update_all_robots !all_robots;
        movement := motion_cycles);

      decr display;
      if !display <= 0 then (
        let open Raylib in
        begin_drawing ();
        clear_background background_color;
        draw_game ();
        draw_endgame result;
        end_drawing ();
        display := update_cycles);

      endgame result

let _ =
  let filenames = Cmd.parse () in
  let programs = List.map (fun f -> (f, read_file f)) filenames in
  let programs =
    match programs with
    | [ p ] -> [ p; p ]
    | ps -> ps
  in
  let programs =
    List.map
      (fun (f, p) ->
        let p = Main.parse p in
        Printf.printf "%s compiled with no errors.\n" f;
        (f, p))
      programs
  in
  let open Robot in
  let robots =
    List.mapi
      (fun i (f, p) ->
        let r = init () in
        cur_robot := r;
        Trace.trace_instr (r.env, r.mem) (Instr p) |> ignore;
        r.name <- f;
        r.program <- p;
        r.x <- Random.int 1000 * click;
        r.last_x <- r.x;
        r.org_x <- r.x;
        r.y <- Random.int 1000 * click;
        r.last_y <- r.y;
        r.org_y <- r.y;
        sprites.(i) <-
          Sprite.create
            (r.x / click |> float_of_int)
            (r.y / click |> float_of_int)
            colors.(i);
        r)
      programs
  in
  all_robots := Array.of_list robots;
  let open Raylib in
  init_window window_width window_height "crobots";

  load_fonts ();

  set_target_fps 60;
  loop ();

  (* allow any flying missile to explode *)
  while
    Array.exists
      (fun r ->
        Array.exists (fun (m : Missile.t) -> m.status <> AVAIL) r.missiles)
      !all_robots
  do
    decr movement;
    if !movement <= 0 then (
      Motion.update_all_robots !all_robots;
      movement := motion_cycles);

    decr display;
    if !display <= 0 then (
      draw_game ();
      display := update_cycles)
  done;

  let winner =
    Array.fold_left
      (fun acc r ->
        match r.status with
        | ALIVE -> Some r
        | _ -> acc)
      None !all_robots
  in

  let winner_msg =
    Option.fold ~none:"It's a tie"
      ~some:(fun r -> Printf.sprintf "%s won the match" r.name)
      winner
  in

  print_endline winner_msg;
  endgame winner_msg
