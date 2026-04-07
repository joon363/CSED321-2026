type action = Tml.exp -> unit

let show e = print_endline (Inout.exp2string e)

let eval action e = action (Eval.multiStep e)

let wait action e =
  let _ = action e;
          print_string "Press return:"; flush_all ();
          input_line stdin
  in ()

let step action e = Seq.iter action (Eval.stepStream e)

(* Running the actions on an interactive loop or a file *)

let loop action = Seq.iter action (Inout.read_line ())

let loopFile name action = Seq.iter action (Inout.read_file name)
