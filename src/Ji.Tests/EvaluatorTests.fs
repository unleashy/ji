namespace Ji.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Ji
open Ji.Evaluator

type EvaluatorTests() =
    let evalInEmptyEnv = eval Env.empty

    [<Property>]
    let ``Evaluates integers`` (num: bigint) =
        num >= 0I
        ==> Assert.Equal(Value.Int num, Expr.Int num |> evalInEmptyEnv)

    [<Fact>]
    let ``Evaluates negation`` () =
        Assert.Equal(
            Value.Int -1234I,
            Expr.Unary(op = UnaryOp.Neg, expr = Expr.Int 1234I)
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates addition`` () =
        Assert.Equal(
            Value.Int(12I + 34I),
            Expr.Binary(
                left = Expr.Int 12I,
                op = BinaryOp.Add,
                right = Expr.Int 34I
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates subtraction`` () =
        Assert.Equal(
            Value.Int(99I - 11I),
            Expr.Binary(
                left = Expr.Int 99I,
                op = BinaryOp.Sub,
                right = Expr.Int 11I
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates multiplication`` () =
        Assert.Equal(
            Value.Int(123I * 123I),
            Expr.Binary(
                left = Expr.Int 123I,
                op = BinaryOp.Mul,
                right = Expr.Int 123I
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates division`` () =
        Assert.Equal(
            Value.Int(80I / 5I),
            Expr.Binary(
                left = Expr.Int 80I,
                op = BinaryOp.Div,
                right = Expr.Int 5I
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
            Value.Int 42I,
            Expr.Call(
                callee = Expr.Function(paramNames = [], body = Expr.Int 42I),
                args = []
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates function calls with a single parameter`` () =
        Assert.Equal(
            Value.Int 9999I,
            Expr.Call(
                callee =
                    Expr.Function(paramNames = [ "x" ], body = Expr.Name "x"),
                args = [ Expr.Int 9999I ]
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates function calls with multiple parameters`` () =
        Assert.Equal(
            Value.Int(2I + 5I - 7I),
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
                args = [ Expr.Int 2I; Expr.Int 5I; Expr.Int 7I ]
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Functions are closures`` () =
        Assert.Equal(
            Value.Int(1I - 2I),
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
                        args = [ Expr.Int 1I ]
                    ),
                args = [ Expr.Int 2I ]
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
                env = (Env.empty |> Env.put [ ("x", Value.Int 5I) ]),
                parameters = [ "y" ],
                body = body
            ),
            // (λx y → x * y) 5 = (λy → 5 * y)
            Expr.Call(
                callee = Expr.Function(paramNames = [ "x"; "y" ], body = body),
                args = [ Expr.Int 5I ]
            )
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Functions in curried form can be called directly`` () =
        Assert.Equal(
            Value.Int(2I - 1I),
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
                args = [ Expr.Int 2I; Expr.Int 1I ]
            )
            |> evalInEmptyEnv
        )
