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
