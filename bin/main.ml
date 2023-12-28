open Crobots
open Sprite
open Gui

let usage_msg = "crobots <robot-programs>"

module Cmd = struct
  let parse () =
    let input_files = ref [] in

    let speclist = [] in
    let anon_fun filename = input_files := filename :: !input_files in
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

let instr_per_update = 20
let clock = ref 0

let draw_game () =
  let open Raylib in
  begin_drawing ();
  clear_background Color.raywhite;

  draw_rectangle_lines padding padding arena_width arena_width Color.gray;

  Array.iteri
    (fun i (r : Robot.t) ->
      let open Sprite in
      update_sprite sprites.(i) r;
      draw_sprite sprites.(i) r;
      draw_stats i r sprites.(i).color)
    Robot.(!all_robots);

  draw_fps (padding + 5) (padding + 5);
  end_drawing ()

let rec loop () =
  let open Robot in
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      Array.iter
        (fun r ->
          match r.status with
          | ALIVE ->
              cur_robot := r;
              r.ep <- Trace.trace1_expr (r.env, r.mem) r.ep;
              Memory.janitor r.env r.mem
          | DEAD -> ())
        !all_robots;

      if !clock = instr_per_update then (
        update_all_robots !all_robots;
        draw_game ();
        clock := 0);

      clock := !clock + 1;
      flush stdout;
      loop ()

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
        r.x <- Random.int 1000;
        r.last_x <- r.x;
        r.org_x <- r.x;
        r.y <- Random.int 1000;
        r.last_y <- r.y;
        r.org_y <- r.y;
        sprites.(i) <-
          Sprite.create (r.x |> float_of_int) (r.y |> float_of_int) colors.(i);
        r)
      programs
  in
  all_robots := Array.of_list robots;
  let open Raylib in
  init_window window_width window_height "crobots";
  set_target_fps 59;
  loop ()
