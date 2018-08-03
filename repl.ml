open Core
open Printf

let rec loop () =
  printf "> ";
  Out_channel.(flush stdout);
  match In_channel.(input_line stdin) with
  | Some line ->
    line |> Lex.lex |> Parse.parse |> Eval.eval |> printf "%d\n";
    loop ()
  | None ->
    print_endline "Goodbye";
    ()

let () = loop ();