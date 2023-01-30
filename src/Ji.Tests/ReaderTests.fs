namespace Ji.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Ji
open Ji.Reader

type GenName =
    static member String() =
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

        Gen.map2 (+) genNameStart genNameContinue |> Arb.fromGen

type ReaderTests() =
    [<Fact>]
    let ``Throws on unknown characters`` () =
        let error = Assert.Throws<JiError>(fun () -> read "´" :> obj)
        Assert.Equal(ErrorCode.UnknownChar, error.Data0.Code)
        Assert.Equal({ Line = 1; Column = 1 }, error.Data0.Location)

    [<Property>]
    let ``Reads integers`` (num: bigint) =
        num >= 0I ==> lazy (Assert.Equal(Expr.Int num, read $"{num}"))

    [<Property>]
    let ``Skips whitespace`` () =
        Gen.elements [ "\r"; "\t"; " " ]
        |> Gen.nonEmptyListOf
        |> Gen.map (String.concat "")
        |> Arb.fromGen
        |> Prop.forAll
        <| fun white -> Assert.Equal(Expr.Int 1I, read $"{white}1{white}")

    [<Property>]
    let ``Skips comments`` ((NonNull s): NonNull<string>) =
        not <| s.Contains('\n')
        ==> lazy (Assert.Equal(Expr.Int 1I, read $"#{s}\n1#{s}"))

    [<Property(Arbitrary = [| typeof<GenName> |])>]
    let ``Reads names`` (name: string) =
        Assert.Equal(Expr.Name name, read $"{name}")

    [<Fact>]
    let ``Reads negations`` () =
        Assert.Equal(
            Expr.Unary(op = UnaryOp.Neg, expr = Expr.Int 1234I),
            read "-1234"
        )

    [<Fact>]
    let ``Reads additive expressions`` () =
        Assert.Equal(
            Expr.Binary(
                left =
                    Expr.Binary(
                        left = Expr.Int 56I,
                        op = BinaryOp.Add,
                        right = Expr.Int 78I
                    ),
                op = BinaryOp.Sub,
                right = Expr.Int 90I
            ),
            read "56 + 78 - 90"
        )

    [<Fact>]
    let ``Reads multiplicative expressions`` () =
        Assert.Equal(
            Expr.Binary(
                left =
                    Expr.Binary(
                        left = Expr.Int 12I,
                        op = BinaryOp.Mul,
                        right = Expr.Int 34I
                    ),
                op = BinaryOp.Div,
                right = Expr.Int 56I
            ),
            read "12 * 34 / 56"
        )

    [<Fact>]
    let ``Reads parenthesised expressions`` () =
        Assert.Equal(
            Expr.Binary(
                left =
                    Expr.Binary(
                        left = Expr.Int 1I,
                        op = BinaryOp.Add,
                        right = Expr.Int 2I
                    ),
                op = BinaryOp.Mul,
                right =
                    Expr.Binary(
                        left = Expr.Int 3I,
                        op = BinaryOp.Sub,
                        right = Expr.Int 4I
                    )
            ),
            read "(1 + 2) * (3 - 4)"
        )

    [<Fact>]
    let ``Throws on unclosed parentheses`` () =
        let error = Assert.Throws<JiError>(fun () -> read "(42 - 42" :> obj)
        Assert.Equal(ErrorCode.UnclosedParens, error.Data0.Code)
        Assert.Equal({ Line = 1; Column = 9 }, error.Data0.Location)

    [<Fact>]
    let ``Throws on extraneous input`` () =
        let error = Assert.Throws<JiError>(fun () -> read "(0))" :> obj)
        Assert.Equal(ErrorCode.ExtraneousInput, error.Data0.Code)
        Assert.Equal({ Line = 1; Column = 4 }, error.Data0.Location)

    [<Fact>]
    let ``Throws if it expected an expression`` () =
        let error = Assert.Throws<JiError>(fun () -> read "1 + +" :> obj)
        Assert.Equal(ErrorCode.ExpectedExpr, error.Data0.Code)
        Assert.Equal({ Line = 1; Column = 5 }, error.Data0.Location)

    [<Fact>]
    let ``Reads functions with no parameters`` () =
        Assert.Equal(
            Expr.Function(paramNames = [], body = Expr.Int 1I),
            read "λ → 1"
        )

    [<Property(EndSize = 5, Arbitrary = [| typeof<GenName> |])>]
    let ``Reads functions with parameters`` (paramNames: string list) =
        let paramNamesCode = paramNames |> String.concat " "
        Assert.Equal(
            Expr.Function(paramNames, body = Expr.Int 99I),
            read $"λ{paramNamesCode} → 99"
        )

    [<Fact>]
    let ``Throws if functions lack the arrow`` () =
        let error = Assert.Throws<JiError>(fun () -> read "λ 99" :> obj)
        Assert.Equal(ErrorCode.ExpectedArrow, error.Data0.Code)
        Assert.Equal({ Line = 1; Column = 3 }, error.Data0.Location)

    [<Fact>]
    let ``Accepts ASCII syntax for functions`` () =
        Assert.Equal(
            Expr.Function(paramNames = [ "a"; "b" ], body = Expr.Int 42I),
            read @"\a b -> 42"
        )

    [<Fact>]
    let ``Reads function calls with one argument`` () =
        Assert.Equal(
            Expr.Call(callee = Expr.Name "f", args = [ Expr.Int 8I ]),
            read "f 8"
        )

    [<Fact>]
    let ``Reads function calls with many arguments`` () =
        Assert.Equal(
            Expr.Call(
                callee = Expr.Name "g",
                args =
                    [ Expr.Int 1I
                      Expr.Unary(op = UnaryOp.Neg, expr = Expr.Int 2I)
                      Expr.Binary(
                          left = Expr.Int 3I,
                          op = BinaryOp.Add,
                          right = Expr.Int 4I
                      ) ]
            ),
            read "g 1 (-2) (3 + 4)"
        )

    [<Fact>]
    let ``Reads function call chains`` () =
        Assert.Equal(
            Expr.Call(
                callee =
                    Expr.Call(callee = Expr.Name "h", args = [ Expr.Int 2I ]),
                args = [ Expr.Int 3I; Expr.Int 4I ]
            ),
            read "(h 2) 3 4"
        )
