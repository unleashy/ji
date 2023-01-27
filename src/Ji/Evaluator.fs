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

let rec eval (env: Env.T) (expr: Expr) : Value<Env.T> =
    match expr with
    | Expr.Int(n) -> Value.Int(n)
    | Expr.Name(name) ->
        match env |> Env.get name with
        | Some(value) -> value
        | None -> failwith $"{name} is not in scope"
    | Expr.Function(paramNames, body) -> Value.Function(env, paramNames, body)
    | Expr.Call(callee, args) ->
        match eval env callee with
        | Value.Function(funEnv, parameters, body) ->
            evalCall funEnv parameters body (args |> List.map (eval env))
        | v -> failwith $"Cannot call non-function value {v}"
    | Expr.Unary(op, expr) -> evalUnary op (eval env expr)
    | Expr.Binary(left, op, right) ->
        evalBinary (eval env left) op (eval env right)

and private evalCall env parameters body args =
    if args.Length <= parameters.Length then
        let boundParams, remainingParams =
            parameters |> List.splitAt args.Length
        let envWithArgs = env |> Env.put (List.zip boundParams args)

        if remainingParams |> List.isEmpty then
            eval envWithArgs body
        else
            Value.Function(
                env = envWithArgs,
                parameters = remainingParams,
                body = body
            )
    else
        let boundArgs, remainingArgs = args |> List.splitAt parameters.Length
        let envWithArgs = env |> Env.put (List.zip parameters boundArgs)

        match eval envWithArgs body with
        | Value.Function(env, parameters, body) ->
            evalCall env parameters body remainingArgs
        | v ->
            failwith $"Cannot call non-function value {v} (too many arguments?)"
