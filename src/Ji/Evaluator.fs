module Ji.Evaluator

open Ji.Ast
open Ji.Value

let private evalUnary op value =
    match (op, value) with
    | (UnaryOp.Neg, ValueInt num) -> ValueInt(-num)

let private evalBinary left op right =
    match (left, op, right) with
    | (ValueInt left, BinaryOp.Add, ValueInt right) -> ValueInt(left + right)
    | (ValueInt left, BinaryOp.Sub, ValueInt right) -> ValueInt(left - right)
    | (ValueInt left, BinaryOp.Mul, ValueInt right) -> ValueInt(left * right)
    | (ValueInt left, BinaryOp.Div, ValueInt right) -> ValueInt(left / right)

let rec eval (expr: Expr) : Value =
    match expr with
    | ExprInt(n) -> ValueInt(n)
    | ExprUnary(op, expr) -> evalUnary op (eval expr)
    | ExprBinary(left, op, right) -> evalBinary (eval left) op (eval right)
