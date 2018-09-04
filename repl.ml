open Core
open Printf

let ctx = [
  ("true", Eval.Bool true);
  ("false", Eval.Bool false)
]
let type_ctx =
  (Map.empty (module String))
  |> Map.add_exn ~key: "true" ~data:Typecheck.Bool
  |> Map.add_exn ~key: "false" ~data:Typecheck.Bool

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
      let expr_type = Typecheck.typeof ast type_ctx in
      ast
      |> (fun x -> Eval.eval x ctx)
      |> Eval.show_expr_type
      |> printf "%s :: %s\n" (Typecheck.show_expr_type expr_type);

      loop ()
    )
  | None ->
    print_endline "Goodbye";
    ()

let () =
  let print_ast = ref false in
  let speclist = [
    ("-r", Arg.Unit loop, "Opens the repl");
    ("-print-ast", Arg.Set print_ast, "Prints the AST on every command");
    ("-c", Arg.Unit (fun () -> (
      let program = In_channel.(input_all stdin) in
      let ast =
        program
        |> Lex.lex
        |> Parse.parse
      in
      Typecheck.typeof ast (Map.empty (module String)) |> ignore;
      ast
      |> Compile_to_js.compile
      |> printf "%s\n";
    )), "Compiles the input to Javascript");
    ("-e", Arg.Unit (fun () -> (
      let program = In_channel.(input_all stdin) in
      let ast =
        program
        |> Lex.lex
        |> Parse.parse
      in

      if !print_ast then
        print_endline @@ Parse.show_ast ast;

      let expr_type = Typecheck.typeof ast type_ctx in
      ast
      |> (fun x -> Eval.eval x ctx)
      |> Eval.show_expr_type
      |> printf "%s :: %s\n" (Typecheck.show_expr_type expr_type);
    )), "Evals the input")
    ] in
  let usage_msg = "The test-lang interpreter and JS compiler." in
  let anonymous_arg_fun =
    fun _ -> () in
  Arg.parse speclist anonymous_arg_fun usage_msg;