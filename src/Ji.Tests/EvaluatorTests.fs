module Ji.Tests.EvaluatorTests

open Xunit
open FsCheck
open FsCheck.Xunit
open Ji.Ast
open Ji.Value
open Ji.Evaluator

[<Property>]
let ``Evaluates integers`` (num: NonNegativeInt) =
    Assert.Equal(ExprInt num.Get |> eval, ValueInt num.Get)

[<Fact>]
let ``Evaluates negation`` () =
    Assert.Equal(
        ExprUnary(op = UnaryOp.Neg, expr = ExprInt 1234) |> eval,
        ValueInt -1234
    )

[<Fact>]
let ``Evaluates addition`` () =
    Assert.Equal(
        ExprBinary(left = ExprInt 12, op = BinaryOp.Add, right = ExprInt 34)
        |> eval,
        ValueInt(int64 (12 + 34))
    )

[<Fact>]
let ``Evaluates subtraction`` () =
    Assert.Equal(
        ExprBinary(left = ExprInt 99, op = BinaryOp.Sub, right = ExprInt 11)
        |> eval,
        ValueInt(int64 (99 - 11))
    )

[<Fact>]
let ``Evaluates multiplication`` () =
    Assert.Equal(
        ExprBinary(left = ExprInt 123, op = BinaryOp.Mul, right = ExprInt 123)
        |> eval,
        ValueInt(int64 (123 * 123))
    )

[<Fact>]
let ``Evaluates division`` () =
    Assert.Equal(
        ExprBinary(left = ExprInt 80, op = BinaryOp.Div, right = ExprInt 5)
        |> eval,
        ValueInt(int64 (80 / 5))
    )
