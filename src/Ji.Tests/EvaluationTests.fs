namespace Ji.Tests

open System.Text.RegularExpressions
open Xunit
open FsCheck
open FsCheck.Xunit
open Ji
open Ji.Reader
open Ji.Evaluator

module CustomGen =
    let private nameStartChars = '_' :: [ 'A' .. 'Z' ] @ [ 'a' .. 'z' ]
    let private nameContinueChars = nameStartChars @ [ '0' .. '9' ]

    let nameStart =
        Gen.elements nameStartChars
        |> Gen.nonEmptyListOf
        |> Gen.map (List.map string >> String.concat "")

    let nameContinue =
        Gen.elements nameContinueChars
        |> Gen.listOf
        |> Gen.map (List.map string >> String.concat "")

    let name = Gen.map2 (+) nameStart nameContinue

type ShouldThrowData =
    { Code: ErrorCode
      Line: int
      Column: int }

type EvaluationTests() =
    let collectSpanTags taggedCode =
        let mutable tags = []
        let code =
            Regex.Replace(
                taggedCode,
                $@"(?<tag>\w+)‹(?<text>.+?)›",
                fun (match_: Match) ->
                    let tag = match_.Groups["tag"]
                    let text = match_.Groups["text"]

                    tags <- (tag.Value, (text.Index, text.Length)) :: tags

                    // Replace the tags and chevrons by the same amount of
                    // spaces so the indices and lengths match up~
                    String.replicate (tag.Length + 1) " " + text.Value + " "
            )

        (code, Map tags)

    let usingTaggedCode taggedCode f =
        let code, spanTags = collectSpanTags taggedCode

        let withSpan tag expr =
            let index, length = spanTags |> Map.find tag
            { Expr = expr
              Span =
                { Code = code
                  Index = index
                  Length = length } }

        f code withSpan

    let shouldEval code value =
        Assert.Equal(value, read code |> eval Env.empty)

    let shouldThrow code (data: ShouldThrowData) =
        let error =
            Assert.Throws<JiException>(fun () ->
                read code |> eval Env.empty :> obj)
        Assert.Equal(error.Code, data.Code)
        Assert.Equal(
            error.Location,
            { Line = data.Line
              Column = data.Column }
        )

    [<Fact>]
    let ``Throws on unknown characters`` () =
        shouldThrow
            @"´"
            { Code = ErrorCode.UnknownChar
              Line = 1
              Column = 1 }

    [<Property>]
    let ``Evaluates integers`` (num: bigint) =
        num >= 0I ==> lazy (shouldEval $@"{num}" (Value.Int num))

    [<Property>]
    let ``Skips whitespace`` () =
        Gen.elements [ "\r"; "\t"; " " ]
        |> Gen.nonEmptyListOf
        |> Gen.map (String.concat "")
        |> Arb.fromGen
        |> Prop.forAll
        <| fun white -> shouldEval $@"{white}1{white}" (Value.Int 1I)

    [<Property>]
    let ``Skips comments`` (NonNull s: NonNull<string>) =
        not <| s.Contains('\n')
        ==> lazy (shouldEval $"#{s}\n42#{s}" (Value.Int 42I))

    [<Fact>]
    let ``Evaluates negations`` () = shouldEval @"-1234" (Value.Int -1234I)

    [<Fact>]
    let ``Evaluates binary expressions`` () =
        shouldEval
            @"12 + 34 - 56 * 78 / 90 * (1 + 1)"
            (Value.Int(12I + 34I - 56I * 78I / 90I * (1I + 1I)))

    [<Fact>]
    let ``Throws on unclosed parentheses`` () =
        shouldThrow
            @"(42 - 42"
            { Code = ErrorCode.UnclosedParens
              Line = 1
              Column = 9 }

    [<Fact>]
    let ``Throws on extraneous input`` () =
        shouldThrow
            @"(0))"
            { Code = ErrorCode.ExtraneousInput
              Line = 1
              Column = 4 }

    [<Fact>]
    let ``Throws if it expected an expression`` () =
        shouldThrow
            @"1 + +"
            { Code = ErrorCode.ExpectedExpr
              Line = 1
              Column = 5 }

    [<Fact>]
    let ``Evaluates functions with no parameters`` () =
        usingTaggedCode @"λ → num‹1›"
        <| fun code withSpanTag ->
            shouldEval
                code
                (Value.Function(
                    env = Env.empty,
                    parameters = [],
                    body = (Expr.Int {| Value = 1I |} |> withSpanTag "num")
                ))

    [<Property(EndSize = 5)>]
    let ``Evaluates functions with parameters`` () =
        CustomGen.name |> Gen.nonEmptyListOf |> Arb.fromGen |> Prop.forAll
        <| fun parameters ->
            let parametersCode = parameters |> String.concat " "
            usingTaggedCode $@"λ{parametersCode} → num‹99›"
            <| fun code withSpanTag ->
                shouldEval
                    code
                    (Value.Function(
                        env = Env.empty,
                        parameters = parameters,
                        body = (Expr.Int {| Value = 99I |} |> withSpanTag "num")
                    ))

    [<Fact>]
    let ``Throws if function lacks the arrow`` () =
        shouldThrow
            @"λ 99"
            { Code = ErrorCode.ExpectedArrow
              Line = 1
              Column = 3 }

    [<Fact>]
    let ``Accepts ASCII syntax for functions`` () =
        usingTaggedCode $@"\a b -> num‹42›"
        <| fun code withSpanTag ->
            shouldEval
                code
                (Value.Function(
                    env = Env.empty,
                    parameters = [ "a"; "b" ],
                    body = (Expr.Int {| Value = 42I |} |> withSpanTag "num")
                ))

    [<Fact>]
    let ``Evaluates function calls with one argument`` () =
        shouldEval @"(λx → x + 1) 8" (Value.Int 9I)

    [<Fact>]
    let ``Evaluates function calls with multiple arguments`` () =
        shouldEval @"(λx y z → x + y - z) 2 5 7" (Value.Int(2I + 5I - 7I))

    [<Fact>]
    let ``Functions are closures`` () =
        shouldEval @"((λx → λy → x - y) 1) 2" (Value.Int(1I - 2I))

    [<Fact>]
    let ``Functions are curried`` () =
        usingTaggedCode @"(λx y → x‹x› * y‹y›) 5"
        <| fun code withSpanTag ->
            shouldEval
                code
                (Value.Function(
                    env = (Env.empty |> Env.put [ ("x", Value.Int 5I) ]),
                    parameters = [ "y" ],
                    body =
                        { Expr =
                            (Expr.Binary
                                {| Left =
                                    Expr.Name {| Value = "x" |}
                                    |> withSpanTag "x"
                                   Op = BinaryOp.Mul
                                   Right =
                                    Expr.Name {| Value = "y" |}
                                    |> withSpanTag "y" |})
                          // We can't nest tags so figure out the index by hand
                          Span = { Code = code; Index = 10; Length = 8 } }
                ))

    [<Fact>]
    let ``Functions in curried form can be called directly`` () =
        shouldEval @"(λx → λy → x - y) 2 1" (Value.Int(2I - 1I))

    [<Fact>]
    let ``Throws on undefined name`` () =
        shouldThrow
            @"(λx → y) 1"
            { Code = ErrorCode.NotInScope
              Line = 1
              Column = 7 }

    [<Fact>]
    let ``Throws if trying to call a non-function`` () =
        shouldThrow
            @"1 2"
            { Code = ErrorCode.InvalidCall
              Line = 1
              Column = 1 }

    [<Fact>]
    let ``Throws if too many arguments are passed to a function`` () =
        shouldThrow
            @"(\x -> x) 1 2"
            { Code = ErrorCode.InvalidCall
              Line = 1
              Column = 1 }

    [<Fact>]
    let ``Throws on invalid type for unary`` () =
        shouldThrow
            @"-(\x -> x)"
            { Code = ErrorCode.InvalidTypeUnary
              Line = 1
              Column = 1 }

    [<Fact>]
    let ``Throws on invalid type for binary`` () =
        shouldThrow
            @"(\x -> x) + 42"
            { Code = ErrorCode.InvalidTypeBinary
              Line = 1
              Column = 1 }
