module Ji.Tests.ReaderTests

open Xunit
open FsCheck
open FsCheck.Xunit
open Ji.Ast
open Ji.Reader

[<Property>]
let ``Reads integers`` (num: NonNegativeInt) =
    Assert.Equal(ExprInt num.Get, read $"{num}")

[<Property>]
let ``Skips whitespace`` () =
    Gen.elements [ "\r"; "\t"; " " ]
    |> Gen.nonEmptyListOf
    |> Gen.map (String.concat "")
    |> Arb.fromGen
    |> Prop.forAll
    <| fun white -> Assert.Equal(ExprInt 1, read $"{white}1{white}")

[<Fact>]
let ``Reads negations`` () =
    Assert.Equal(ExprUnary(op = UnaryOp.Neg, expr = ExprInt 1234), read "-1234")

[<Fact>]
let ``Reads additive expressions`` () =
    Assert.Equal(
        ExprBinary(
            left =
                ExprBinary(
                    left = ExprInt 56,
                    op = BinaryOp.Add,
                    right = ExprInt 78
                ),
            op = BinaryOp.Sub,
            right = ExprInt 90
        ),
        read "56 + 78 - 90"
    )

[<Fact>]
let ``Reads multiplicative expressions`` () =
    Assert.Equal(
        ExprBinary(
            left =
                ExprBinary(
                    left = ExprInt 12,
                    op = BinaryOp.Mul,
                    right = ExprInt 34
                ),
            op = BinaryOp.Div,
            right = ExprInt 56
        ),
        read "12 * 34 / 56"
    )

[<Fact>]
let ``Reads parenthesised expressions`` () =
    Assert.Equal(
        ExprBinary(
            left =
                ExprBinary(
                    left = ExprInt 1,
                    op = BinaryOp.Add,
                    right = ExprInt 2
                ),
            op = BinaryOp.Mul,
            right =
                ExprBinary(
                    left = ExprInt 3,
                    op = BinaryOp.Sub,
                    right = ExprInt 4
                )
        ),
        read "(1 + 2) * (3 - 4)"
    )
