open Crobots

let usage_msg = "crobots <robot-program>"

module Cmd = struct
  let parse () =
    let input_file = ref None in

    let speclist = [] in
    let anon_fun filename = input_file := Some filename in
    Arg.parse speclist anon_fun usage_msg;

    match !input_file with
    | None ->
        Arg.usage speclist usage_msg;
        print_endline "missing program, goodbye";
        exit 0
    | Some f -> f
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

let loop () =
  let instr_per_update = 20 in
  let clock = ref 0 in
  while true do
    Robot.cur_robot.ep <- Trace.trace1_expr Robot.cur_robot.ep;
    (* Printf.sprintf "[cycle: %d] %s" !clock
      (Prettyprint.string_of_expr Robot.cur_robot.ep)
    |> print_endline; *)
    Prettyprint.string_of_robot Robot.cur_robot |> print_endline;

    if !clock = instr_per_update then (
      Robot.update_robot Robot.cur_robot;
      clock := 0);
    clock := !clock + 1;

    Unix.sleepf 0.01
  done

let _ =
  let filename = Cmd.parse () in
  let program = read_file filename in
  let p = Main.parse program in
  Printf.printf "speed %d\n" Robot.cur_robot.speed;
  Memory.init ();
  Trace.trace_instr (Instr p) |> ignore;
  Robot.cur_robot.name <- filename;
  Robot.cur_robot.program <- p;
  Robot.cur_robot.ep <- Ast.(CALL ("main", []));
  loop ()
