open Core
open Lex

type op =
  | Sum
  | Subtraction
  | Multiplication
  | Division
  | GreaterThan
  | LesserThan
[@@ deriving show]

type label = string
[@@deriving show]

type ast =
  | Variable of label
  | Digit of int
  | Expr of ast * op * ast
  | LetExpr of label * ast * ast
  | Parenthesised of ast
  | IfElseExpr of ast * ast * ast
[@@deriving show]

(*
  expr ::= comp (> comp)?
  comp ::= factor (sum_sub factor)*
  factor ::= term (mul_div term)*
  term ::=
    digit |
    variable |
    '(' expr ')' |
    'let' label '=' expr 'in' expr |
    'if' expr 'then' expr 'else' expr

 *)
let parse (input: lexeme list): ast =
  let op_of_lexeme l = match l with
    | Plus -> Sum
    | Minus -> Subtraction
    | Star -> Multiplication
    | Slash -> Division
    | Greater -> GreaterThan
    | Lesser -> LesserThan
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
    let e1 = comp () in
    match peek () with
    | Some Greater | Some Lesser as cmp_op -> (
        let cmp_op = Option.value_exn cmp_op in
        consume cmp_op;
        let e2 = comp () in
        Expr (e1, (op_of_lexeme cmp_op), e2)
      )
    | _ -> e1
  and comp (): ast =
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
    | Some If -> (
        consume If;
        let e1 = expr () in
        consume Then;
        let e2 = expr () in
        consume Else;
        let e3 = expr () in
        IfElseExpr (e1, e2, e3)
      )
    | Some other -> failwith @@ "expected Number or LParen or Let or Id, got " ^ (show_lexeme other)
    | None -> failwith "stream finished"

  in expr ()
