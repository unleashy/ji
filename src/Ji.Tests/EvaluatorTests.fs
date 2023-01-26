namespace Ji.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Ji
open Ji.Ast
open Ji.Values
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
            Value.Function(parameters = [ "x" ], body = Expr.Name "x"),
            // λ(x) → x
            Expr.Function(paramNames = [ "x" ], body = Expr.Name "x")
            |> evalInEmptyEnv
        )
