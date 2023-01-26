namespace Ji.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Ji.Ast
open Ji.Reader

type ReaderTests() =
    [<Property>]
    let ``Reads integers`` (num: NonNegativeInt) =
        Assert.Equal(Expr.Int num.Get, read $"{num}")

    [<Property>]
    let ``Skips whitespace`` () =
        Gen.elements [ "\r"; "\t"; " " ]
        |> Gen.nonEmptyListOf
        |> Gen.map (String.concat "")
        |> Arb.fromGen
        |> Prop.forAll
        <| fun white -> Assert.Equal(Expr.Int 1, read $"{white}1{white}")

    let nameStartChars = '_' :: [ 'A' .. 'Z' ] @ [ 'a' .. 'z' ]
    let nameContinueChars = nameStartChars @ [ '0' .. '9' ]

    let genNameStart =
        Gen.elements nameStartChars
        |> Gen.nonEmptyListOf
        |> Gen.map (List.map string >> String.concat "")

    let genNameContinue =
        Gen.elements nameContinueChars
        |> Gen.listOf
        |> Gen.map (List.map string >> String.concat "")

    let genName = Gen.map2 (+) genNameStart genNameContinue

    [<Property>]
    let ``Reads names`` () =
        genName |> Arb.fromGen |> Prop.forAll
        <| fun name -> Assert.Equal(Expr.Name name, read $"{name}")

    [<Fact>]
    let ``Reads negations`` () =
        Assert.Equal(
            Expr.Unary(op = UnaryOp.Neg, expr = Expr.Int 1234),
            read "-1234"
        )

    [<Fact>]
    let ``Reads additive expressions`` () =
        Assert.Equal(
            Expr.Binary(
                left =
                    Expr.Binary(
                        left = Expr.Int 56,
                        op = BinaryOp.Add,
                        right = Expr.Int 78
                    ),
                op = BinaryOp.Sub,
                right = Expr.Int 90
            ),
            read "56 + 78 - 90"
        )

    [<Fact>]
    let ``Reads multiplicative expressions`` () =
        Assert.Equal(
            Expr.Binary(
                left =
                    Expr.Binary(
                        left = Expr.Int 12,
                        op = BinaryOp.Mul,
                        right = Expr.Int 34
                    ),
                op = BinaryOp.Div,
                right = Expr.Int 56
            ),
            read "12 * 34 / 56"
        )

    [<Fact>]
    let ``Reads parenthesised expressions`` () =
        Assert.Equal(
            Expr.Binary(
                left =
                    Expr.Binary(
                        left = Expr.Int 1,
                        op = BinaryOp.Add,
                        right = Expr.Int 2
                    ),
                op = BinaryOp.Mul,
                right =
                    Expr.Binary(
                        left = Expr.Int 3,
                        op = BinaryOp.Sub,
                        right = Expr.Int 4
                    )
            ),
            read "(1 + 2) * (3 - 4)"
        )

    [<Fact>]
    let ``Reads functions with no parameters`` () =
        Assert.Equal(
            Expr.Function(paramNames = [], body = Expr.Int 1),
            read "λ → 1"
        )

    [<Property(EndSize = 5)>]
    let ``Reads functions with parameters`` () =
        genName |> Gen.listOf |> Arb.fromGen |> Prop.forAll
        <| fun paramNames ->
            let paramNamesCode = paramNames |> String.concat ", "
            Assert.Equal(
                Expr.Function(paramNames, body = Expr.Int 99),
                read $"λ({paramNamesCode}) → 99"
            )

    [<Fact>]
    let ``Reads function calls with no arguments`` () =
        Assert.Equal(
            Expr.Call(
                callee = Expr.Function(paramNames = [], body = Expr.Int 1),
                args = []
            ),
            read "(λ() → 1)()"
        )

    [<Fact>]
    let ``Reads function calls with one argument`` () =
        Assert.Equal(
            Expr.Call(callee = Expr.Name "f", args = [ Expr.Int 8 ]),
            read "f(8)"
        )

    [<Fact>]
    let ``Reads function calls with many arguments`` () =
        Assert.Equal(
            Expr.Call(
                callee = Expr.Name "g",
                args =
                    [ Expr.Int 1
                      Expr.Unary(op = UnaryOp.Neg, expr = Expr.Int 2)
                      Expr.Binary(
                          left = Expr.Int 3,
                          op = BinaryOp.Add,
                          right = Expr.Int 4
                      ) ]
            ),
            read "g(1, -2, 3 + 4)"
        )

    [<Fact>]
    let ``Reads function call chains`` () =
        Assert.Equal(
            Expr.Call(
                callee =
                    Expr.Call(
                        callee = Expr.Call(callee = Expr.Name "h", args = []),
                        args = [ Expr.Int 2 ]
                    ),
                args = [ Expr.Int 3; Expr.Int 4 ]
            ),
            read "h()(2)(3, 4)"
        )
