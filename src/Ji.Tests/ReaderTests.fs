namespace Ji.Tests

open System.Text.RegularExpressions
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
    let rec printAst (expr: SpannedExpr) : string =
        let exprStr =
            match expr.Expr with
            | Expr.Int expr -> $"Int {expr.Value}"
            | Expr.Name expr -> $"Name {expr.Value}"
            | Expr.Function expr ->
                let parameters = expr.Parameters |> String.concat " "
                $"Function ({parameters}) {printAst expr.Body}"
            | Expr.Call expr ->
                let args = expr.Args |> List.map printAst |> String.concat " "
                $"Call {printAst expr.Callee} {args}"
            | Expr.Unary expr -> $"Unary {expr.Op} {printAst expr.Expr}"
            | Expr.Binary expr ->
                $"Binary {printAst expr.Left} {expr.Op} {printAst expr.Right}"

        $"({exprStr}):{expr.Span.Index},{expr.Span.Length}"

    let uglify (s: string) : string =
        Regex.Replace(
            Regex.Replace(s.Trim(), @"\s+", " "),
            @"\s+\)", // fix spacing before paren close
            ")"
        )

    [<Fact>]
    let ``Throws on unknown characters`` () =
        let error = Assert.Throws<JiError>(fun () -> read "´" :> obj)
        Assert.Equal(ErrorCode.UnknownChar, error.Data0.Code)
        Assert.Equal({ Line = 1; Column = 1 }, error.Data0.Location)

    [<Property>]
    let ``Reads integers`` (num: bigint) =
        num >= 0I
        ==> lazy
            (let code = $"{num}"
             let actual = read code
             Assert.Equal($@"(Int {num}):0,{code.Length}", printAst actual))

    [<Property>]
    let ``Skips whitespace`` () =
        Gen.elements [ "\r"; "\t"; " " ]
        |> Gen.nonEmptyListOf
        |> Gen.map (String.concat "")
        |> Arb.fromGen
        |> Prop.forAll
        <| fun white ->
            let code = $"{white}1{white}"
            let actual = read code
            Assert.Equal($@"(Int 1):{white.Length},1", printAst actual)

    [<Property>]
    let ``Skips comments`` ((NonNull s): NonNull<string>) =
        not <| s.Contains('\n')
        ==> lazy
            (let code = $"#{s}\n1#{s}"
             let actual = read code
             Assert.Equal($@"(Int 1):{s.Length + 2},1", printAst actual))

    [<Property(Arbitrary = [| typeof<GenName> |])>]
    let ``Reads names`` (name: string) =
        let code = $"{name}"
        let actual = read code
        Assert.Equal($@"(Name {name}):0,{name.Length}", printAst actual)

    [<Fact>]
    let ``Reads negations`` () =
        let code = "-1234"
        let actual = read code
        Assert.Equal(@"(Unary Neg (Int 1234):1,4):0,5", printAst actual)

    [<Fact>]
    let ``Reads additive expressions`` () =
        let code = "56 + 78 - 90"
        let actual = read code
        Assert.Equal(
            uglify
                @"
                (Binary
                    (Binary (Int 56):0,2 Add (Int 78):5,2):0,7
                    Sub
                    (Int 90):10,2
                ):0,12
                ",
            printAst actual
        )

    [<Fact>]
    let ``Reads multiplicative expressions`` () =
        let code = "12 * 34 / 56"
        let actual = read code
        Assert.Equal(
            uglify
                @"
                (Binary
                    (Binary (Int 12):0,2 Mul (Int 34):5,2):0,7
                    Div
                    (Int 56):10,2
                ):0,12
                ",
            printAst actual
        )

    [<Fact>]
    let ``Reads parenthesised expressions`` () =
        let code = "(1 + 2) * (3 - 4)"
        let actual = read code
        Assert.Equal(
            uglify
                @"
                (Binary
                    (Binary (Int 1):1,1 Add (Int 2):5,1):0,7
                    Mul
                    (Binary (Int 3):11,1 Sub (Int 4):15,1):10,7
                ):0,17
                ",
            printAst actual
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
        let code = "λ → 1"
        let actual = read code
        Assert.Equal(
            uglify
                @"
                (Function () (Int 1):4,1):0,5
                ",
            printAst actual
        )

    [<Property(EndSize = 5, Arbitrary = [| typeof<GenName> |])>]
    let ``Reads functions with parameters`` (parameters: string list) =
        let parametersCode = parameters |> String.concat " "
        let code = $"λ{parametersCode} → 99"

        let actual = read code

        Assert.Equal(
            $@"(Function ({parametersCode}) (Int 99):{parametersCode.Length + 4},2):0,{code.Length}",
            printAst actual
        )

    [<Fact>]
    let ``Throws if functions lack the arrow`` () =
        let error = Assert.Throws<JiError>(fun () -> read "λ 99" :> obj)
        Assert.Equal(ErrorCode.ExpectedArrow, error.Data0.Code)
        Assert.Equal({ Line = 1; Column = 3 }, error.Data0.Location)

    [<Fact>]
    let ``Accepts ASCII syntax for functions`` () =
        let code = @"\a b -> 42"
        let actual = read code
        Assert.Equal(
            uglify @"(Function (a b) (Int 42):8,2):0,10",
            printAst actual
        )

    [<Fact>]
    let ``Reads function calls with one argument`` () =
        let code = "f 8"
        let actual = read code
        Assert.Equal(
            uglify @"(Call (Name f):0,1 (Int 8):2,1):0,3",
            printAst actual
        )

    [<Fact>]
    let ``Reads function calls with many arguments`` () =
        let code = "g 1 (-2) (3 + 4)"
        let actual = read code
        Assert.Equal(
            uglify
                @"
                (Call (Name g):0,1
                    (Int 1):2,1
                    (Unary Neg (Int 2):6,1):4,4
                    (Binary (Int 3):10,1 Add (Int 4):14,1):9,7
                ):0,16
                ",
            printAst actual
        )

    [<Fact>]
    let ``Reads function call chains`` () =
        let code = "(h 2) 3 4"
        let actual = read code
        Assert.Equal(
            uglify
                @"
                (Call
                    (Call
                        (Name h):1,1
                        (Int 2):3,1
                    ):0,5
                    (Int 3):6,1
                    (Int 4):8,1
                ):0,9
                ",
            printAst actual
        )
