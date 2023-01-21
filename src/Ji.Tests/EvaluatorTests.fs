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
