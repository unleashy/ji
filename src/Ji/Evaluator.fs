namespace Ji

module Evaluator =
    let private evalUnary op value span =
        match op, value with
        | UnaryOp.Neg, Value.Int(num) -> Value.Int(-num)
        | _ ->
            Error.raiseWith
                { Code = ErrorCode.InvalidTypeUnary
                  Message =
                    "Cannot apply unary operator "
                    + Printer.printUnaryOp op
                    + " to a value of type "
                    + Printer.printType value
                  Span = span }

    let private evalBinary left op right span =
        match left, op, right with
        | Value.Int left, BinaryOp.Add, Value.Int right ->
            Value.Int(left + right)
        | Value.Int left, BinaryOp.Sub, Value.Int right ->
            Value.Int(left - right)
        | Value.Int left, BinaryOp.Mul, Value.Int right ->
            Value.Int(left * right)
        | Value.Int left, BinaryOp.Div, Value.Int right ->
            Value.Int(left / right)
        | _ ->
            Error.raiseWith
                { Code = ErrorCode.InvalidTypeBinary
                  Message =
                    "Cannot apply binary operator "
                    + Printer.printBinaryOp op
                    + " to values of type "
                    + Printer.printType left
                    + " and "
                    + Printer.printType right
                  Span = span }

    let rec eval
        (env: Env)
        ({ Expr = expr; Span = span }: SpannedExpr)
        : Value<Env> =
        match expr with
        | Expr.Int(expr) -> Value.Int(expr.Value)

        | Expr.Name(expr) ->
            match env |> Env.get expr.Value with
            | Some(value) -> value
            | None ->
                Error.raiseWith
                    { Code = ErrorCode.NotInScope
                      Message = $"{expr.Value} is not in scope"
                      Span = span }

        | Expr.Function(expr) -> Value.Function(env, expr.Parameters, expr.Body)

        | Expr.Call(expr) ->
            match eval env expr.Callee with
            | Value.Function(funEnv, parameters, body) ->
                evalCall
                    funEnv
                    parameters
                    body
                    (expr.Args |> List.map (eval env))
                    span

            | v ->
                Error.raiseWith
                    { Code = ErrorCode.InvalidCall
                      Message =
                        $"Cannot call non-function value of type {v |> Printer.printType}"
                      Span = span }

        | Expr.Unary(expr) -> evalUnary expr.Op (eval env expr.Expr) span

        | Expr.Binary(expr) ->
            evalBinary (eval env expr.Left) expr.Op (eval env expr.Right) span

    and private evalCall env parameters body args span =
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
                evalCall env parameters body remainingArgs span
            | v ->
                Error.raiseWith
                    { Code = ErrorCode.InvalidCall
                      Message =
                        "Cannot call non-function value of type "
                        + Printer.printType v
                        + ", maybe you passed too many arguments?"
                      Span = span }
