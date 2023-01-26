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

    | Expr.Name(name) ->
        match env |> Env.get name with
        | Some(value) -> value
        | None -> failwith $"{name} is not in scope"

    | Expr.Function(paramNames, body) -> Value.Function(paramNames, body)

    | Expr.Call(callee, args) ->
        match eval env callee with
        | Value.Function(parameters, body) ->
            if parameters.Length = args.Length then
                let evalArgs = args |> List.map (eval env)
                let funEnv = env |> Env.put (List.zip parameters evalArgs)
                eval funEnv body
            else
                failwith
                    $"Expected {parameters.Length} arguments, but only {args.Length} were provided"
        | _ -> failwith $"Cannot call non-function value {callee}"

    | Expr.Unary(op, expr) -> evalUnary op (eval env expr)

    | Expr.Binary(left, op, right) ->
        evalBinary (eval env left) op (eval env right)
