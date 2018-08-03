open Core
open Printf

let rec loop () =
  Out_channel.(flush stdout);
  match In_channel.(input_line stdin) with
  | Some line ->
    line |> Lex.lexer |> Parse.parse |> Eval.eval |> printf "%d\n";
    loop ()
  | None -> ()

let () = loop ();