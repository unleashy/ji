module Ji.Evaluator

open Ji.Ast
open Ji.Values

let private evalUnary op value =
    match (op, value) with
    | (UnaryOp.Neg, Value.Int(num)) -> Value.Int(-num)
    | _ -> failwith $"Cannot apply unary operator {op} to a value like {value}"

let private evalBinary left op right =
    match (left, op, right) with
    | (Value.Int left, BinaryOp.Add, Value.Int right) -> Value.Int(left + right)
    | (Value.Int left, BinaryOp.Sub, Value.Int right) -> Value.Int(left - right)
    | (Value.Int left, BinaryOp.Mul, Value.Int right) -> Value.Int(left * right)
    | (Value.Int left, BinaryOp.Div, Value.Int right) -> Value.Int(left / right)
    | _ ->
        failwith
            $"Cannot apply binary operator {op} to values {left} and {right}"

let rec eval (env: Env.T) (expr: Expr) : Value =
    match expr with
    | Expr.Int(n) -> Value.Int(n)
    | Expr.Name(n) -> raise (System.NotImplementedException())
    | Expr.Function(paramNames, body) -> Value.Function(paramNames, body)
    | Expr.Unary(op, expr) -> evalUnary op (eval env expr)
    | Expr.Call _ -> raise (System.NotImplementedException())
    | Expr.Binary(left, op, right) ->
        evalBinary (eval env left) op (eval env right)
