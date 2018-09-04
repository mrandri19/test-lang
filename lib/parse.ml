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

type type_ =
  | Literal of string
  | Unit
  | Arrow of type_ * type_
  | Tuple_ of type_ * type_
[@@deriving show]

type ast =
  | Variable of label
  | Digit of int
  | Unit
  | Expr of ast * op * ast
  | LetExpr of label * ast * ast
  | Parenthesised of ast
  | IfElseExpr of ast * ast * ast
  | FunDecl of label * type_ * ast
  | FunApp of ast * ast
  | Tuple_ of ast * ast
  | TupleAccess of ast * int
[@@deriving show]

(*
  expr = tup (, tup)?
  tup ::= comp (> comp)?
  comp ::= factor (sum_sub factor)*
  factor ::= dot (mul_div dot)*
  dot :: = app (. digit)?
  app ::= term term*
  term ::=
    digit |
    variable |
    '(' expr ')' |
    'let' label '=' expr 'in' expr |
    'if' expr 'then' expr 'else' expr |
    'fun' arg+ '->' expr |

  type ::=
    prod ('->' prod)* |
  prod ::= label ('*' label)?

  arg ::=  label ':' type
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

  let _debug_tokens () =
    print_endline (!tokens |> List.map ~f:Lex.show_lexeme |> String.concat ~sep:", ")
  in

  let peek () =
    List.hd !tokens
  in

  let consume ?(exn_constructor = (fun t -> Failure t)) (tok: lexeme) : unit =
    match List.hd !tokens with
    | Some t ->
      if t = tok then
        tokens := List.tl_exn !tokens
      else
        raise @@ exn_constructor @@
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
    let e1 = tup () in
    match peek () with
    | Some Comma ->
      consume Comma;
      let e2 = tup () in
      Tuple_(e1, e2)
    | _ -> e1
  and tup (): ast =
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
    let ter = dot () in
    let tmp = ref ter in

    while (match peek () with | Some Star | Some Slash -> true | _ -> false) do
      let op = Option.value_exn (peek () ) in
      consume op;
      let ex = dot () in
      tmp := Expr (!tmp, op |> op_of_lexeme, ex)
    done;
    !tmp
  and dot (): ast =
    let e1 = app () in
    match peek () with
    | Some Dot ->(
      consume Dot;
      match peek () with
      | Some (Number n) ->
        consume (Number n);
        TupleAccess(e1, n)
      | _ -> failwith "Expected digit after dot"
    )
    | _ -> e1
  and app (): ast =
    let e1 = term () in

    let tmp = ref e1 in

    let continue = ref true in
    while !continue do
      let backtrack = !tokens in
      try
        let e2 = term () in
        tmp := FunApp (!tmp, e2);
      with
      | _ -> (
          (* Backtrack in case we cant match any more expressions *)
          tokens := backtrack;
          continue := false;
        )
    done;
    !tmp
  and term (): ast =
    match peek () with
    | Some Lex.Unit -> consume Lex.Unit; Unit
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
        | _ -> failwith "expected Identifier"
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
    | Some Fun -> (
        consume Fun;

        let module Local = struct
          exception NoColon
          (* exception NoTypeId *)
          (* exception NoArg *)
        end in
        let open Local in

        let rec type__ (): type_ =
          let tmp = prod () in

          (*
            TODO: use this pattern to implement the kleene star in other places
            too, it's nice and doesn't use any whiles and refs
           *)
          match peek () with
          | Some Arrow -> (
            consume Arrow;
            let ty = type__ () in
            Arrow (tmp, ty)
          )
          | _ -> tmp

        and prod (): type_ =
          let tmp = lit_or_unit () in
          match peek () with
          | Some Star ->
            consume Star;
            let t2 = lit_or_unit () in
            Tuple_ (tmp, t2)
          | _ -> tmp

        and lit_or_unit (): type_ =
          match peek () with
          | Some Id(_)
          | Some Unit as typ -> (
            let typ = Option.value_exn typ in
            consume typ;

            match typ with
            | Id(typename) ->Literal typename
            | Unit -> Unit
            | _ -> failwith "wtf"
          )
          | _ -> failwith "type must start with identifier"
        in

        let arg () =
          match peek () with
          | Some Id(argname) -> (
              consume (Id argname);

              consume ~exn_constructor:(fun _ -> raise NoColon) (Colon);

              consume LParen;
              let ty = type__ () in
              consume RParen;

              (argname, ty)
            )
          | _ -> failwith "expected argument identifier"
        in

        let args = ref [arg ()] in
        let continue = ref true in
        while !continue do
          let backtrack = !tokens in
          try
            args := (arg ())::!args;
          with
            (* TODO: finish to implement decent parse errors *)
            | Failure _error -> (
              tokens := backtrack;
              continue := false;
            )
            | NoColon -> (
              failwith @@ "Expected the start of type declaration ':' for example, got: '" ^ show_lexeme (List.hd_exn !tokens) ^ "' instead"
            )
            (* | NoTypeId -> (
              failwith @@ "Expected a type 'int' for example, got: '" ^ show_lexeme (List.hd_exn !tokens) ^ "' instead"
            ) *)
        done;

        consume (Arrow);
        let e1 = expr () in
        (* args right now is in reverse order:
           `fun x:int y:int z:int -> ...`
           args is [(z,int); (y,int), (x:int)]
           but this is good since then folding it will make it become
           0: e1
           1: fun z:int -> e1
           2: fun y:int -> fun z:int -> e1
           3: fun x:int -> fun y:int -> fun z:int -> e1
        *)
        List.fold !args ~init:e1 ~f:(fun acc (argname, argtype) -> (FunDecl(argname, argtype,acc)))
      )
    | Some other -> failwith @@ "expected a term expansion, got " ^ (show_lexeme other)
    | None -> failwith "stream finished"

  in
  let syntax_tree = expr () in
  match !tokens with
  | [] -> syntax_tree
  | rest -> failwith @@
    "lexemes not empty, still got: [ "
    ^ (rest |> List.map ~f:show_lexeme |> String.concat ~sep:", " )
    ^ " ]"
    ^ ", currently syntax tree is: "
    ^ show_ast syntax_tree