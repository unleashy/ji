module Ji.Tests.EvaluatorTests

open Xunit
open FsCheck
open FsCheck.Xunit
open Ji.Ast
open Ji.Value
open Ji.Evaluator

[<Property>]
let ``Evaluates integers`` (num: NonNegativeInt) =
    Assert.Equal(ValueInt num.Get, ExprInt num.Get |> eval)

[<Fact>]
let ``Evaluates negation`` () =
    Assert.Equal(
        ValueInt -1234,
        ExprUnary(op = UnaryOp.Neg, expr = ExprInt 1234) |> eval
    )

[<Fact>]
let ``Evaluates addition`` () =
    Assert.Equal(
        ValueInt(int64 (12 + 34)),
        ExprBinary(left = ExprInt 12, op = BinaryOp.Add, right = ExprInt 34)
        |> eval
    )

[<Fact>]
let ``Evaluates subtraction`` () =
    Assert.Equal(
        ValueInt(int64 (99 - 11)),
        ExprBinary(left = ExprInt 99, op = BinaryOp.Sub, right = ExprInt 11)
        |> eval
    )

[<Fact>]
let ``Evaluates multiplication`` () =
    Assert.Equal(
        ValueInt(int64 (123 * 123)),
        ExprBinary(left = ExprInt 123, op = BinaryOp.Mul, right = ExprInt 123)
        |> eval
    )

[<Fact>]
let ``Evaluates division`` () =
    Assert.Equal(
        ValueInt(int64 (80 / 5)),
        ExprBinary(left = ExprInt 80, op = BinaryOp.Div, right = ExprInt 5)
        |> eval
    )
