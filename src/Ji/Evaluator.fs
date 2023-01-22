module Ji.Evaluator

open Ji.Ast
open Ji.Value

let evalUnary (op: UnaryOp.T) (value: Value) : Value =
    match (op, value) with
    | (UnaryOp.Neg, ValueInt num) -> ValueInt(-num)
// | _ -> failwith $"Cannot apply unary operator {op} to value {value}"

let rec eval (expr: Expr) : Value =
    match expr with
    | ExprInt(n) -> ValueInt(n)
    | ExprUnary(op, expr) -> evalUnary op (eval expr)
    | _ -> failwith $"Unknown AST {expr}"
