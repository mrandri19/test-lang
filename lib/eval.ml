open Base

module P = Parse

let rec eval (ast: P.ast) (ctx: (string,int,String.comparator_witness) Map.t): int =
  match ast with
  | P.Digit d -> d
  | P.Expr (lhs, op, rhs) ->(
      match op with
      | P.Sum -> (eval lhs ctx) + (eval rhs ctx)
      | P.Subtraction -> (eval lhs ctx) - (eval rhs ctx)
      | P.Multiplication -> (eval lhs ctx) * (eval rhs ctx)
      | P.Division -> (eval lhs ctx) / (eval rhs ctx))
  | P.Parenthesised e -> eval e ctx
  | P.LetExpr (label, e1, e2) ->(
      let new_ctx = Map.set ctx ~key:label ~data:(eval e1 ctx) in
      eval e2 new_ctx
    )
  | P.Variable label -> Map.find_exn ctx label
