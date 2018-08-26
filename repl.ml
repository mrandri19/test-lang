open Core
open Printf

let rec loop () =
  printf "> ";
  Out_channel.(flush stdout);
  match In_channel.(input_line stdin) with
  | Some line ->(
      let ast =
        line
        |> Lex.lex
        |> Parse.parse
      in
      let _expr_type = Typecheck.typecheck ast in
      ast
      (* |> (fun x -> Eval.eval x [])
         |> Eval.show_expr_type
         |> printf "%s :: %s\n" (Typecheck.show_expr_type expr_type); *)
      |> Compile_to_js.compile
      |> printf "%s\n";
      loop ()
    )
  | None ->
    print_endline "Goodbye";
    ()

let () = loop ();