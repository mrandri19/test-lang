module P = Parse

let compile ast =
  let rec compile (ast: P.ast): string =
    match ast with
    | P.Unit -> "null"
    | P.Digit d -> string_of_int d
    | P.Tuple_ (e1, e2) -> "[" ^ compile e1 ^ "," ^ compile e2 ^ "]"
    | P.TupleAccess (e1, n) -> "(" ^ compile e1 ^ ")" ^ "["^ string_of_int n ^"]"
    | P.Variable label -> label
    | P.Expr (e1, op, e2) -> (
        let string_of_op = function
          | P.Sum -> "+"
          | P.Subtraction -> "-"
          | P.Multiplication -> "*"
          | P.Division -> "/"
          | P.GreaterThan -> ">"
          | P.LesserThan -> "<"
        in
        (compile e1) ^ (string_of_op op) ^ (compile e2)
      )
    | P.LetExpr (label, e1, e2) -> (
        (* FIXME: this doesn't work  *)
        "{" ^ "const " ^ label ^ " = " ^ (compile e1) ^ "; return (" ^ (compile e2) ^ ")}"
      )
    | P.Parenthesised e1 -> "(" ^ (compile e1) ^ ")"
    | P.IfElseExpr(e1,e2,e3) -> (
        (compile e1) ^ " ? " ^ (compile e2) ^ " : " ^ (compile e3)
      )
    | P.FunDecl (label, _type, e1) -> (
        label ^ " => " ^ (compile e1)
      )
    | P.FunApp (e1, e2) -> (
        (compile e1) ^ "(" ^ (compile e2) ^ ")"
    )
  in "console.log((function() {" ^ compile ast ^ "})())"