open Core
open Printf

let rec loop () =
  printf "> ";
  Out_channel.(flush stdout);
  match In_channel.(input_line stdin) with
  | Some line ->
    line
    |> Lex.lex
    |> Parse.parse
    |> (fun x -> Eval.eval x (Map.empty (module String)))
    |> Eval.show_expr_type
    |> printf "%s\n";
    loop ()
  | None ->
    print_endline "Goodbye";
    ()

let () = loop ();