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
        num >= 0I
        ==> lazy (Assert.Equal(Expr.Int {| Value = num |}, read $"{num}"))

    [<Property>]
    let ``Skips whitespace`` () =
        Gen.elements [ "\r"; "\t"; " " ]
        |> Gen.nonEmptyListOf
        |> Gen.map (String.concat "")
        |> Arb.fromGen
        |> Prop.forAll
        <| fun white ->
            Assert.Equal(Expr.Int {| Value = 1I |}, read $"{white}1{white}")

    [<Property>]
    let ``Skips comments`` ((NonNull s): NonNull<string>) =
        not <| s.Contains('\n')
        ==> lazy (Assert.Equal(Expr.Int {| Value = 1I |}, read $"#{s}\n1#{s}"))

    [<Property(Arbitrary = [| typeof<GenName> |])>]
    let ``Reads names`` (name: string) =
        Assert.Equal(Expr.Name {| Value = name |}, read $"{name}")

    [<Fact>]
    let ``Reads negations`` () =
        Assert.Equal(
            Expr.Unary
                {| Op = UnaryOp.Neg
                   Expr = Expr.Int {| Value = 1234I |} |},
            read "-1234"
        )

    [<Fact>]
    let ``Reads additive expressions`` () =
        Assert.Equal(
            Expr.Binary
                {| Left =
                    Expr.Binary
                        {| Left = Expr.Int {| Value = 56I |}
                           Op = BinaryOp.Add
                           Right = Expr.Int {| Value = 78I |} |}
                   Op = BinaryOp.Sub
                   Right = Expr.Int {| Value = 90I |} |},
            read "56 + 78 - 90"
        )

    [<Fact>]
    let ``Reads multiplicative expressions`` () =
        Assert.Equal(
            Expr.Binary
                {| Left =
                    Expr.Binary
                        {| Left = Expr.Int {| Value = 12I |}
                           Op = BinaryOp.Mul
                           Right = Expr.Int {| Value = 34I |} |}
                   Op = BinaryOp.Div
                   Right = Expr.Int {| Value = 56I |} |},
            read "12 * 34 / 56"
        )

    [<Fact>]
    let ``Reads parenthesised expressions`` () =
        Assert.Equal(
            Expr.Binary
                {| Left =
                    Expr.Binary
                        {| Left = Expr.Int {| Value = 1I |}
                           Op = BinaryOp.Add
                           Right = Expr.Int {| Value = 2I |} |}
                   Op = BinaryOp.Mul
                   Right =
                    Expr.Binary
                        {| Left = Expr.Int {| Value = 3I |}
                           Op = BinaryOp.Sub
                           Right = Expr.Int {| Value = 4I |} |} |},
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
            Expr.Function
                {| Parameters = []
                   Body = Expr.Int {| Value = 1I |} |},
            read "λ → 1"
        )

    [<Property(EndSize = 5, Arbitrary = [| typeof<GenName> |])>]
    let ``Reads functions with parameters`` (paramNames: string list) =
        let paramNamesCode = paramNames |> String.concat " "
        Assert.Equal(
            Expr.Function
                {| Parameters = paramNames
                   Body = Expr.Int {| Value = 99I |} |},
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
            Expr.Function
                {| Parameters = [ "a"; "b" ]
                   Body = Expr.Int {| Value = 42I |} |},
            read @"\a b -> 42"
        )

    [<Fact>]
    let ``Reads function calls with one argument`` () =
        Assert.Equal(
            Expr.Call
                {| Callee = Expr.Name {| Value = "f" |}
                   Args = [ Expr.Int {| Value = 8I |} ] |},
            read "f 8"
        )

    [<Fact>]
    let ``Reads function calls with many arguments`` () =
        Assert.Equal(
            Expr.Call
                {| Callee = Expr.Name {| Value = "g" |}
                   Args =
                    [ Expr.Int {| Value = 1I |}
                      Expr.Unary
                          {| Op = UnaryOp.Neg
                             Expr = Expr.Int {| Value = 2I |} |}
                      Expr.Binary
                          {| Left = Expr.Int {| Value = 3I |}
                             Op = BinaryOp.Add
                             Right = Expr.Int {| Value = 4I |} |} ] |},
            read "g 1 (-2) (3 + 4)"
        )

    [<Fact>]
    let ``Reads function call chains`` () =
        Assert.Equal(
            Expr.Call
                {| Callee =
                    Expr.Call
                        {| Callee = Expr.Name {| Value = "h" |}
                           Args = [ Expr.Int {| Value = 2I |} ] |}
                   Args =
                    [ Expr.Int {| Value = 3I |}; Expr.Int {| Value = 4I |} ] |},
            read "(h 2) 3 4"
        )
