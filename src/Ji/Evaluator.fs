module Ji.Evaluator

open Ji.Ast
open Ji.Values

let private evalUnary op value =
    match (op, value) with
    | (UnaryOp.Neg, Value.Int(num)) -> Value.Int(-num)

let private evalBinary left op right =
    match (left, op, right) with
    | (Value.Int left, BinaryOp.Add, Value.Int right) -> Value.Int(left + right)
    | (Value.Int left, BinaryOp.Sub, Value.Int right) -> Value.Int(left - right)
    | (Value.Int left, BinaryOp.Mul, Value.Int right) -> Value.Int(left * right)
    | (Value.Int left, BinaryOp.Div, Value.Int right) -> Value.Int(left / right)

let rec eval (expr: Expr) : Value =
    match expr with
    | Expr.Int(n) -> Value.Int(n)
    | Expr.Unary(op, expr) -> evalUnary op (eval expr)
    | Expr.Binary(left, op, right) -> evalBinary (eval left) op (eval right)
