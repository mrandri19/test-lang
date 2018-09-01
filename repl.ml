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
      let expr_type = Typecheck.typecheck ast in
      ast
      |> (fun x -> Eval.eval x [])
         |> Eval.show_expr_type
         |> printf "%s :: %s\n" (Typecheck.show_expr_type expr_type);
      (* |> Compile_to_js.compile
      |> printf "%s\n"; *)
      loop ()
    )
  | None ->
    print_endline "Goodbye";
    ()

let () =
  let speclist = [
    ("-r", Arg.Unit loop, "Opens the repl");
    ("-c", Arg.Unit (fun () -> (
      let program = In_channel.(input_all stdin) in
      let ast =
        program
        |> Lex.lex
        |> Parse.parse
      in
      Typecheck.typecheck ast |> ignore;
      ast
      |> Compile_to_js.compile
      |> printf "%s\n";
    )), "Compiles the input to Javascript");
    ] in
  let usage_msg = "The test-lang interpreter and JS compiler." in
  let anonymous_arg_fun =
    fun _ -> () in
  Arg.parse speclist anonymous_arg_fun usage_msg;