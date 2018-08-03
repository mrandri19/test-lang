module P = Parse
let rec eval (ast: P.ast): int =
  match ast with
  | P.Digit d -> d
  | P.Expr (lhs, op, rhs) ->(
      match op with
      | P.Sum -> (eval lhs) + (eval rhs)
      | P.Subtraction -> (eval lhs) - (eval rhs)
      | P.Multiplication -> (eval lhs) * (eval rhs)
      | P.Division -> (eval lhs) / (eval rhs))
  | P.Parenthesised e -> eval e
