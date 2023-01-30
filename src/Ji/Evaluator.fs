namespace Ji

module Evaluator =
    let private evalUnary op value =
        match (op, value) with
        | (UnaryOp.Neg, Value.Int(num)) -> Value.Int(-num)
        | _ ->
            failwith $"Cannot apply unary operator {op} to a value like {value}"

    let private evalBinary left op right =
        match (left, op, right) with
        | (Value.Int left, BinaryOp.Add, Value.Int right) ->
            Value.Int(left + right)
        | (Value.Int left, BinaryOp.Sub, Value.Int right) ->
            Value.Int(left - right)
        | (Value.Int left, BinaryOp.Mul, Value.Int right) ->
            Value.Int(left * right)
        | (Value.Int left, BinaryOp.Div, Value.Int right) ->
            Value.Int(left / right)
        | _ ->
            failwith
                $"Cannot apply binary operator {op} to values {left} and {right}"

    let rec eval (env: Env) (expr: Expr) : Value<Env> =
        match expr with
        | Expr.Int(expr) -> Value.Int(expr.Value)

        | Expr.Name(expr) ->
            match env |> Env.get expr.Value with
            | Some(value) -> value
            | None -> failwith $"{expr.Value} is not in scope"

        | Expr.Function(expr) -> Value.Function(env, expr.Parameters, expr.Body)

        | Expr.Call(expr) ->
            match eval env expr.Callee with
            | Value.Function(funEnv, parameters, body) ->
                evalCall
                    funEnv
                    parameters
                    body
                    (expr.Args |> List.map (eval env))

            | v -> failwith $"Cannot call non-function value {v}"

        | Expr.Unary(expr) -> evalUnary expr.Op (eval env expr.Expr)

        | Expr.Binary(expr) ->
            evalBinary (eval env expr.Left) expr.Op (eval env expr.Right)

    and private evalCall env parameters body args =
        if args.Length = parameters.Length then
            // normal application
            let envWithArgs = env |> Env.put (List.zip parameters args)
            eval envWithArgs body
        elif args.Length < parameters.Length then
            // partial application
            let boundParams, remainingParams =
                parameters |> List.splitAt args.Length
            let envWithArgs = env |> Env.put (List.zip boundParams args)

            Value.Function(
                env = envWithArgs,
                parameters = remainingParams,
                body = body
            )
        else
            // curried application
            let boundArgs, remainingArgs =
                args |> List.splitAt parameters.Length
            let envWithArgs = env |> Env.put (List.zip parameters boundArgs)

            match eval envWithArgs body with
            | Value.Function(env, parameters, body) ->
                evalCall env parameters body remainingArgs
            | v ->
                failwith
                    $"Cannot call non-function value {v} (too many arguments?)"
