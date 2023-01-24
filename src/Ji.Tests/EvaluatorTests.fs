module Ji.Tests.EvaluatorTests

open Xunit
open FsCheck
open FsCheck.Xunit
open Ji.Ast
open Ji.Values
open Ji.Evaluator

[<Property>]
let ``Evaluates integers`` (num: NonNegativeInt) =
    Assert.Equal(Value.Int num.Get, Expr.Int num.Get |> eval)

[<Fact>]
let ``Evaluates negation`` () =
    Assert.Equal(
        Value.Int -1234,
        Expr.Unary(op = UnaryOp.Neg, expr = Expr.Int 1234) |> eval
    )

[<Fact>]
let ``Evaluates addition`` () =
    Assert.Equal(
        Value.Int(int64 (12 + 34)),
        Expr.Binary(left = Expr.Int 12, op = BinaryOp.Add, right = Expr.Int 34)
        |> eval
    )

[<Fact>]
let ``Evaluates subtraction`` () =
    Assert.Equal(
        Value.Int(int64 (99 - 11)),
        Expr.Binary(left = Expr.Int 99, op = BinaryOp.Sub, right = Expr.Int 11)
        |> eval
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
        |> eval
    )

[<Fact>]
let ``Evaluates division`` () =
    Assert.Equal(
        Value.Int(int64 (80 / 5)),
        Expr.Binary(left = Expr.Int 80, op = BinaryOp.Div, right = Expr.Int 5)
        |> eval
    )
