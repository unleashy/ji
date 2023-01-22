module Ji.Tests.ReaderTests

open Xunit
open FsCheck
open FsCheck.Xunit
open Ji.Ast
open Ji.Reader

[<Property>]
let ``Reads integers`` (num: NonNegativeInt) =
    Assert.Equal(read $"{num}", ExprInt num.Get)

[<Property>]
let ``Skips whitespace`` () =
    Gen.elements [ "\r"; "\t"; " " ]
    |> Gen.nonEmptyListOf
    |> Gen.map (String.concat "")
    |> Arb.fromGen
    |> Prop.forAll
    <| fun white -> Assert.Equal(read $"{white}1{white}", ExprInt 1)

[<Fact>]
let ``Reads negations`` () =
    Assert.Equal(read "-1234", ExprUnary(op = UnaryOp.Neg, expr = ExprInt 1234))
