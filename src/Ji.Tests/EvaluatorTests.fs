namespace Ji.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Ji
open Ji.Evaluator

type EvaluatorTests() =
    let evalInEmptyEnv = eval Env.empty

    [<Property>]
    let ``Evaluates integers`` (num: NonNegativeInt) =
        Assert.Equal(Value.Int num.Get, Expr.Int num.Get |> evalInEmptyEnv)

    [<Fact>]
    let ``Evaluates negation`` () =
        Assert.Equal(
            Value.Int -1234,
            Expr.Unary(op = UnaryOp.Neg, expr = Expr.Int 1234) |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates addition`` () =
        Assert.Equal(
            Value.Int(int64 (12 + 34)),
            Expr.Binary(
                left = Expr.Int 12,
                op = BinaryOp.Add,
                right = Expr.Int 34
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates subtraction`` () =
        Assert.Equal(
            Value.Int(int64 (99 - 11)),
            Expr.Binary(
                left = Expr.Int 99,
                op = BinaryOp.Sub,
                right = Expr.Int 11
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates multiplication`` () =
        Assert.Equal(
            Value.Int(int64 (123 * 123)),
            Expr.Binary(
                left = Expr.Int 123,
                op = BinaryOp.Mul,
                right = Expr.Int 123
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates division`` () =
        Assert.Equal(
            Value.Int(int64 (80 / 5)),
            Expr.Binary(
                left = Expr.Int 80,
                op = BinaryOp.Div,
                right = Expr.Int 5
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates functions`` () =
        Assert.Equal(
            Value.Function(
                env = Env.empty,
                parameters = [ "x" ],
                body = Expr.Name "x"
            ),
            // λ(x) → x
            Expr.Function(paramNames = [ "x" ], body = Expr.Name "x")
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates function calls with no parameters`` () =
        Assert.Equal(
            Value.Int 42,
            Expr.Call(
                callee = Expr.Function(paramNames = [], body = Expr.Int 42),
                args = []
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates function calls with a single parameter`` () =
        Assert.Equal(
            Value.Int 9999,
            Expr.Call(
                callee =
                    Expr.Function(paramNames = [ "x" ], body = Expr.Name "x"),
                args = [ Expr.Int 9999 ]
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates function calls with multiple parameters`` () =
        Assert.Equal(
            Value.Int(2L + 5L - 7L),
            // (λx y z → x + y - z) 2 5 7
            Expr.Call(
                callee =
                    Expr.Function(
                        paramNames = [ "x"; "y"; "z" ],
                        body =
                            Expr.Binary(
                                left =
                                    Expr.Binary(
                                        left = Expr.Name "x",
                                        op = BinaryOp.Add,
                                        right = Expr.Name "y"
                                    ),
                                op = BinaryOp.Sub,
                                right = Expr.Name "z"
                            )
                    ),
                args = [ Expr.Int 2; Expr.Int 5; Expr.Int 7 ]
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Functions are closures`` () =
        Assert.Equal(
            Value.Int(1L - 2L),
            // ((λx → λy → x - y) 1) 2
            Expr.Call(
                callee =
                    Expr.Call(
                        callee =
                            Expr.Function(
                                paramNames = [ "x" ],
                                body =
                                    Expr.Function(
                                        paramNames = [ "y" ],
                                        body =
                                            Expr.Binary(
                                                left = Expr.Name "x",
                                                op = BinaryOp.Sub,
                                                right = Expr.Name "y"
                                            )
                                    )
                            ),
                        args = [ Expr.Int 1 ]
                    ),
                args = [ Expr.Int 2 ]
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Functions are curried`` () =
        let body =
            Expr.Binary(
                left = Expr.Name "x",
                op = BinaryOp.Mul,
                right = Expr.Name "y"
            )

        Assert.Equal(
            Value.Function(
                env = (Env.empty |> Env.put [ ("x", Value.Int 5) ]),
                parameters = [ "y" ],
                body = body
            ),
            // (λx y → x * y) 5 = (λy → 5 * y)
            Expr.Call(
                callee = Expr.Function(paramNames = [ "x"; "y" ], body = body),
                args = [ Expr.Int 5 ]
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Functions in curried form can be called directly`` () =
        Assert.Equal(
            Value.Int(2L - 1L),
            // (λx → λy → x - y) 2 1
            Expr.Call(
                callee =
                    Expr.Function(
                        paramNames = [ "x" ],
                        body =
                            Expr.Function(
                                paramNames = [ "y" ],
                                body =
                                    Expr.Binary(
                                        left = Expr.Name "x",
                                        op = BinaryOp.Sub,
                                        right = Expr.Name "y"
                                    )
                            )
                    ),
                args = [ Expr.Int 2; Expr.Int 1 ]
            )
            |> evalInEmptyEnv
        )
