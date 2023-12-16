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

let _ =
  (* read argument file *)
  let filename = Cmd.parse () in

  let program = read_file filename in 
  Main.parse program
  (* initialize a new robot *)
  (* initialize battlefield *)
  (* initialize missile list *)
  (* robot loop:
      fetch robot instruction
      decode intrinsic
      execute intrinsic
      make due updates to robot *)
