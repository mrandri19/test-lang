open Core
open Lex

type op =
  | Sum
  | Subtraction
  | Multiplication
  | Division
[@@deriving show]

type label = string
[@@deriving show]

type ast =
  | Variable of label
  | Digit of int
  | Expr of ast * op * ast
  | Parenthesised of ast
  | LetExpr of label * ast * ast
[@@deriving show]

(*
  expr ::= factor (sum_sub factor)*
  factor ::= term (mul_div term)*
  term ::= digit | '(' expr ')' | variable | 'let' label '=' expr 'in' expr | variable
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
    | Some Let ->(
        consume Let;
        match peek () with
        | Some Id(label) ->(
            consume (Id label);
            consume Equals;
            let e1 = expr () in
            consume In;
            let e2 = expr () in
            LetExpr (label, e1, e2)
          )
        | _ -> failwith "expeceted Identifier"
      )
    | Some Id(label) -> (
        consume (Id label);
        Variable label
      )
    | Some other -> failwith @@ "expected Number or LParen, got " ^ (show_lexeme other)
    | None -> failwith "stream finished"

  in expr ()
