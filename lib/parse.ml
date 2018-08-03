open Core
open Lex

type op =
  | Sum
  | Subtraction
  | Multiplication
  | Division
[@@deriving show]

type ast =
  | Digit of int
  | Expr of ast * op * ast
  | Parenthesised of ast
[@@deriving show]

(*
  This is the original grammar
  expr ::=
        expr op expr
      | number
      |'(' expr ')'

  We are using a recursive descent approach to parsing so the left
  recursion must be factored out thus making the final grammar like
  this:
  expr ::=
        number (op expr)*
      |'(' expr ')'

  Wrong: this grammar doesnt support things like:
  (1)-1


  The new one needs to be like this:
  expr ::= term (op expr)*
  term ::= (number | '(' expr ')' )

  TODO: implement precedences
  The new grammar with precedences:

  expr ::= factor (sum_sub factor)*
  factor ::= term (mul_div term)*
  term ::= (number | '(' expr ')' )
 *)
let parse (input: lexeme list): ast =
  let op_of_lexeme l = match l with
    | Plus -> Sum
    | Minus -> Subtraction
    | Star -> Multiplication
    | Slash -> Division
    | _ -> failwith (show_lexeme l ^ " can't be converted to op") in

  let tokens = ref input in

  let peek () =
    List.hd !tokens
  in

  let consume tok: unit =
    match List.hd !tokens with
    | Some t ->
      if t = tok then
        tokens := List.tl_exn !tokens
      else
        failwith @@
        "Couldn't consume"
        ^ show_lexeme tok
        ^ " got "
        ^ show_lexeme (List.hd_exn !tokens)
        ^ " instead"
    | None ->
      failwith @@
      "Couldn't consume"
      ^ show_lexeme tok
      ^ " no more tokens left"
  in

  let rec expr (): ast =
    let fac = factor () in
    let tmp = ref fac in

    while (match peek () with | Some Plus | Some Minus -> true | _ -> false) do
      let op = Option.value_exn (peek () ) in
      consume op;
      let ex = factor () in
      tmp := Expr (!tmp, op |> op_of_lexeme, ex)
    done;
    !tmp
  and factor (): ast =
    let ter = term () in
    let tmp = ref ter in

    while (match peek () with | Some Star | Some Slash -> true | _ -> false) do
      let op = Option.value_exn (peek () ) in
      consume op;
      let ex = term () in
      tmp := Expr (!tmp, op |> op_of_lexeme, ex)
    done;
    !tmp
  and term (): ast =
    match peek () with
    | Some Number(n) -> consume (Number n); Digit n
    | Some LParen ->(
        consume LParen;
        let ex = expr () in
        consume RParen;
        Parenthesised ex
      )
    | Some other -> failwith @@ "expected Number or LParen, got " ^ (show_lexeme other)
    | None -> failwith "stream finished"

  in expr ()
