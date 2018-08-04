open Core

type expr_type = Bool | Int
[@@deriving show]

type context = (string,expr_type,String.comparator_witness) Map.t

module P = Parse

let typecheck (ast: Parse.ast): unit =
  let rec typeof ast ctx: expr_type =
    match ast with
    | P.Digit _ -> Int
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
        match (t1,t2) with
        | (Bool,Bool) -> Bool
        | (Int,Int) -> Int
        | _ -> failwith "If-then-else arms have different types"
      )
  in
  typeof ast (Map.empty (module String)) |> ignore;
  (* TODO: add better reporting instead of relying on exceptions *)