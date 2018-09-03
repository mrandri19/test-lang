open Core

module P = Parse

type type_ = string
[@@deriving show]
type name= string
[@@deriving show]

type expr_type =
  | Unit
  | Bool of bool
  | Int of int
  | Closure of context * name * type_ * P.ast
  | Tuple_ of expr_type * expr_type
[@@deriving show]
and context = (name*expr_type) list

let rec eval (ast: P.ast) (ctx: context): expr_type =
  match ast with
  | P.Unit -> Unit
  | P.Digit d -> Int d
  | P.Tuple_ (e1, e2) -> Tuple_ (eval e1 ctx, eval e2 ctx)
  | P.TupleAccess (e1, n) -> (
    match (eval e1 ctx) with
    | Tuple_(e1,e2) -> (
      match n with
      | 1 -> e1
      | 2 -> e2
      | _ -> failwith "Only tuples are supported, not n-uples (use 1 or 2)"
    )
    | _ -> failwith "Expected tuple in tuple access"
  )
  | P.Expr (lhs, op, rhs) -> (
      match (eval lhs ctx, op, eval rhs ctx) with
      | (Int k1, P.Sum, Int k2) -> Int (k1 + k2)
      | (Int k1, P.Subtraction, Int k2) -> Int (k1 - k2)
      | (Int k1, P.Multiplication, Int k2) -> Int (k1 * k2)
      | (Int k1, P.Division, Int k2) -> Int (k1 / k2)
      | (Int k1, P.GreaterThan, Int k2) -> Bool (k1 > k2)
      | (Int k1, P.LesserThan, Int k2) -> Bool (k1 < k2)
      |_ -> failwith "Expected int or bool in binary expression"
    )
  | P.Parenthesised e -> eval e ctx
  | P.LetExpr (label, e1, e2) -> eval e2 ((label,(eval e1 ctx))::ctx)
  | P.Variable label -> (
      match List.Assoc.find ctx ~equal:(=) label with
      | Some v -> v
      | None -> failwith @@ "key: '" ^ label ^"' not found, " ^ show_context ctx
    )
  | P.IfElseExpr (cond, e1, e2) -> (
      match (eval cond ctx) with
      |Bool true -> (eval e1 ctx)
      |Bool false -> (eval e2 ctx)
      | _ as e -> failwith @@ "Expected bool in if expression got: " ^ (show_expr_type e)
    )
  | P.FunDecl (argname, argtype, e1) -> Closure (ctx, argname, argtype, e1)
  | P.FunApp (func, arg) -> (
      match (eval func ctx) with
      | Closure (clos_ctx, argname, _, e1) ->
        eval e1 ((argname,(eval arg ctx))::clos_ctx)
      | _ -> failwith "cannot apply to something that's not a function"
    )
