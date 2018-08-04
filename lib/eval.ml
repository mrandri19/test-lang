open Core

module P = Parse

type expr_type =
  | Bool of bool
  | Int of int
[@@deriving show]

let int_of_expr_type_exn =
  function
  | Int i -> i
  | _ as e -> failwith @@ "Expected int, got " ^ show_expr_type e
let bool_of_expr_type_exn =
  function
  | Bool b -> b
  | _ as e -> failwith @@ "Expected bool, got " ^ show_expr_type e

type scope = (string,expr_type,String.comparator_witness) Map.t

let rec eval (ast: P.ast) (ctx: scope): expr_type =
  match ast with
  | P.Digit d -> Int d
  | P.Expr (lhs, op, rhs) -> (
      match op with
      | P.Sum -> Int (
          int_of_expr_type_exn (eval lhs ctx) +
          int_of_expr_type_exn(eval rhs ctx)
        )
      | P.Subtraction -> Int (
          int_of_expr_type_exn (eval lhs ctx) -
          int_of_expr_type_exn(eval rhs ctx)
        )
      | P.Multiplication -> Int (
          int_of_expr_type_exn (eval lhs ctx) *
          int_of_expr_type_exn(eval rhs ctx)
        )
      | P.Division -> Int (
          int_of_expr_type_exn (eval lhs ctx) /
          int_of_expr_type_exn(eval rhs ctx)
        )
      | P.GreaterThan -> Bool (
          int_of_expr_type_exn (eval lhs ctx) >
          int_of_expr_type_exn(eval rhs ctx)
        )
      | P.LesserThan -> Bool (
          int_of_expr_type_exn (eval lhs ctx) <
          int_of_expr_type_exn (eval rhs ctx)
        )
    )
  | P.Parenthesised e -> eval e ctx
  | P.LetExpr (label, e1, e2) ->(
      let new_ctx = Map.set ctx ~key:label ~data:(eval e1 ctx) in
      eval e2 new_ctx
    )
  | P.Variable label -> Map.find_exn ctx label
  | P.IfElseExpr (cond, e1, e2) -> (
      if bool_of_expr_type_exn (eval cond ctx) then (eval e1 ctx) else (eval e2 ctx)
    )
