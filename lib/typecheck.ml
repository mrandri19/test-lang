open Core

type expr_type =
  | Bool
  | Unit
  | Int
  | Function of expr_type * expr_type
  | Tuple_ of expr_type * expr_type

let rec show_expr_type = function
  | Bool -> "bool"
  | Unit -> "()"
  | Int -> "int"
  | Function (t1, t2) -> show_expr_type t1 ^ " -> " ^ show_expr_type t2
  | Tuple_ (t1, t2) -> "(" ^ show_expr_type t1 ^ ", " ^ show_expr_type t2 ^ ")"

type context = (string,expr_type,String.comparator_witness) Map.t

module P = Parse

let rec type_of_string s =
  match s with
  | P.Literal "int" -> Int
  | P.Literal "bool" -> Bool
  | P.Unit -> Unit
  | P.Arrow (ty1, ty2) -> Function (type_of_string ty1, type_of_string ty2)
  | P.Tuple_ (ty1, ty2) -> Tuple_ (type_of_string ty1, type_of_string ty2)
  | _ -> failwith "unknown type"

let rec typeof ast ctx: expr_type =
    match ast with
    | P.TupleAccess (e1, n) -> (
      let t1 = typeof e1 ctx in
      match t1 with
      | Tuple_ (t2, t3) -> (
        match n with
        | 1 -> t2
        | 2 -> t3
        | _ -> failwith "only tuples are supported, not n-uples (use 1 or 2)"
      )
      | _ -> failwith "Cannot access something that's not a tuple"
    )
    | P.Unit -> Unit
    | P.Digit _ -> Int
    | P.Tuple_ (e1, e2) -> Tuple_ (typeof e1 ctx, typeof e2 ctx)
    | P.Parenthesised e -> typeof e ctx
    | P.Variable label -> Map.find_exn ctx label
    | P.Expr (e1, op, e2) -> (
        let t1 = typeof e1 ctx in
        let t2 = typeof e2 ctx in
        match op with
        | P.Sum | P.Subtraction | P.Multiplication | P.Division -> (
            match (t1,t2) with
            | (Int,Int) -> Int
            | (Int,_) -> failwith "Rhs should have type Int"
            | (_,Int) -> failwith "Lhs should have type Int"
            | _ -> failwith "Neither Lhs nor Rhs have type Int"
          )
        | P.GreaterThan | P.LesserThan -> (
            match (t1,t2) with
            | (Int,Int) -> Bool
            | (Int,_) -> failwith "Rhs should have type Int"
            | (_,Int) -> failwith "Lhs should have type Int"
            | _ -> failwith @@ "Neither Lhs nor Rhs have type Int"
          )
      )
    | P.LetExpr (label, e1, e2) -> (
        let new_ctx = Map.set ctx ~key:label ~data:(typeof e1 ctx) in
        typeof e2 new_ctx
      )
    | P.IfElseExpr (cond, e1, e2) -> (
        let tcond = typeof cond ctx in
        if tcond <> Bool then failwith "if condition needs to be a boolean";
        let t1 = typeof e1 ctx in
        let t2 = typeof e2 ctx in
        if t1 = t2 then
          t1
        else
          failwith "If-then-else arms have different types"
      )
    | P.FunDecl (argname, argtype_s, e1) -> (
        let argtype = type_of_string argtype_s in
        let new_ctx = Map.set ctx ~key:argname ~data:argtype in
        Function (argtype, typeof e1 new_ctx)
      )
    | P.FunApp (fname, farg) -> (
        let ftype = typeof fname ctx in
        let fargtype = typeof farg ctx in
        match ftype with
        | Function (a,b) -> (
            if a = fargtype then
              b
            else
              failwith "mismatched function application types"
          )
        | _ -> failwith "cannot apply to a non function"
      )
(* TODO: add better reporting instead of relying on exceptions *)