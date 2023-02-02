namespace Ji.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Ji
open Ji.Evaluator

type EvaluatorTests() =
    let withDefaultSpan expr =
        { Expr = expr
          Span = { Code = ""; Index = 0; Length = 1 } }

    let evalInEmptyEnv = eval Env.empty

    [<Property>]
    let ``Evaluates integers`` (num: bigint) =
        num >= 0I
        ==> lazy
            Assert.Equal(
                Value.Int num,
                Expr.Int {| Value = num |} |> withDefaultSpan |> evalInEmptyEnv
            )

    [<Fact>]
    let ``Evaluates negation`` () =
        Assert.Equal(
            Value.Int -1234I,
            Expr.Unary
                {| Op = UnaryOp.Neg
                   Expr = Expr.Int {| Value = 1234I |} |> withDefaultSpan |}
            |> withDefaultSpan
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates addition`` () =
        Assert.Equal(
            Value.Int(12I + 34I),
            Expr.Binary
                {| Left = Expr.Int {| Value = 12I |} |> withDefaultSpan
                   Op = BinaryOp.Add
                   Right = Expr.Int {| Value = 34I |} |> withDefaultSpan |}
            |> withDefaultSpan
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates subtraction`` () =
        Assert.Equal(
            Value.Int(99I - 11I),
            Expr.Binary
                {| Left = Expr.Int {| Value = 99I |} |> withDefaultSpan
                   Op = BinaryOp.Sub
                   Right = Expr.Int {| Value = 11I |} |> withDefaultSpan |}
            |> withDefaultSpan
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates multiplication`` () =
        Assert.Equal(
            Value.Int(123I * 123I),
            Expr.Binary
                {| Left = Expr.Int {| Value = 123I |} |> withDefaultSpan
                   Op = BinaryOp.Mul
                   Right = Expr.Int {| Value = 123I |} |> withDefaultSpan |}
            |> withDefaultSpan
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates division`` () =
        Assert.Equal(
            Value.Int(80I / 5I),
            Expr.Binary
                {| Left = Expr.Int {| Value = 80I |} |> withDefaultSpan
                   Op = BinaryOp.Div
                   Right = Expr.Int {| Value = 5I |} |> withDefaultSpan |}
            |> withDefaultSpan
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates functions`` () =
        Assert.Equal(
            Value.Function(
                env = Env.empty,
                parameters = [ "x" ],
                body = (Expr.Name {| Value = "x" |} |> withDefaultSpan)
            ),
            // λ(x) → x
            Expr.Function
                {| Parameters = [ "x" ]
                   Body = Expr.Name {| Value = "x" |} |> withDefaultSpan |}
            |> withDefaultSpan
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates function calls with no parameters`` () =
        Assert.Equal(
            Value.Int 42I,
            Expr.Call
                {| Callee =
                    Expr.Function
                        {| Parameters = []
                           Body = Expr.Int {| Value = 42I |} |> withDefaultSpan |}
                    |> withDefaultSpan
                   Args = [] |}
            |> withDefaultSpan
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates function calls with a single parameter`` () =
        Assert.Equal(
            Value.Int 9999I,
            Expr.Call
                {| Callee =
                    Expr.Function
                        {| Parameters = [ "x" ]
                           Body = Expr.Name {| Value = "x" |} |> withDefaultSpan |}
                    |> withDefaultSpan
                   Args = [ Expr.Int {| Value = 9999I |} |> withDefaultSpan ] |}
            |> withDefaultSpan
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Evaluates function calls with multiple parameters`` () =
        Assert.Equal(
            Value.Int(2I + 5I - 7I),
            // (λx y z → x + y - z) 2 5 7
            Expr.Call
                {| Callee =
                    Expr.Function
                        {| Parameters = [ "x"; "y"; "z" ]
                           Body =
                            Expr.Binary
                                {| Left =
                                    Expr.Binary
                                        {| Left =
                                            Expr.Name {| Value = "x" |}
                                            |> withDefaultSpan
                                           Op = BinaryOp.Add
                                           Right =
                                            Expr.Name {| Value = "y" |}
                                            |> withDefaultSpan |}
                                    |> withDefaultSpan
                                   Op = BinaryOp.Sub
                                   Right =
                                    Expr.Name {| Value = "z" |}
                                    |> withDefaultSpan |}
                            |> withDefaultSpan |}
                    |> withDefaultSpan
                   Args =
                    [ Expr.Int {| Value = 2I |} |> withDefaultSpan
                      Expr.Int {| Value = 5I |} |> withDefaultSpan
                      Expr.Int {| Value = 7I |} |> withDefaultSpan ] |}
            |> withDefaultSpan
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Functions are closures`` () =
        Assert.Equal(
            Value.Int(1I - 2I),
            // ((λx → λy → x - y) 1) 2
            Expr.Call
                {| Callee =
                    Expr.Call
                        {| Callee =
                            Expr.Function
                                {| Parameters = [ "x" ]
                                   Body =
                                    Expr.Function
                                        {| Parameters = [ "y" ]
                                           Body =
                                            Expr.Binary
                                                {| Left =
                                                    Expr.Name {| Value = "x" |}
                                                    |> withDefaultSpan
                                                   Op = BinaryOp.Sub
                                                   Right =
                                                    Expr.Name {| Value = "y" |}
                                                    |> withDefaultSpan |}
                                            |> withDefaultSpan |}
                                    |> withDefaultSpan |}
                            |> withDefaultSpan
                           Args =
                            [ Expr.Int {| Value = 1I |} |> withDefaultSpan ] |}
                    |> withDefaultSpan
                   Args = [ Expr.Int {| Value = 2I |} |> withDefaultSpan ] |}
            |> withDefaultSpan
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Functions are curried`` () =
        let body =
            Expr.Binary
                {| Left = Expr.Name {| Value = "x" |} |> withDefaultSpan
                   Op = BinaryOp.Mul
                   Right = Expr.Name {| Value = "y" |} |> withDefaultSpan |}
            |> withDefaultSpan

        Assert.Equal(
            Value.Function(
                env = (Env.empty |> Env.put [ ("x", Value.Int 5I) ]),
                parameters = [ "y" ],
                body = body
            ),
            // (λx y → x * y) 5 = (λy → 5 * y)
            Expr.Call
                {| Callee =
                    Expr.Function
                        {| Parameters = [ "x"; "y" ]
                           Body = body |}
                    |> withDefaultSpan
                   Args = [ Expr.Int {| Value = 5I |} |> withDefaultSpan ] |}
            |> withDefaultSpan
            |> evalInEmptyEnv
        )

    [<Fact>]
    let ``Functions in curried form can be called directly`` () =
        Assert.Equal(
            Value.Int(2I - 1I),
            // (λx → λy → x - y) 2 1
            Expr.Call
                {| Callee =
                    Expr.Function
                        {| Parameters = [ "x" ]
                           Body =
                            Expr.Function
                                {| Parameters = [ "y" ]
                                   Body =
                                    Expr.Binary
                                        {| Left =
                                            Expr.Name {| Value = "x" |}
                                            |> withDefaultSpan
                                           Op = BinaryOp.Sub
                                           Right =
                                            Expr.Name {| Value = "y" |}
                                            |> withDefaultSpan |}
                                    |> withDefaultSpan |}
                            |> withDefaultSpan |}
                    |> withDefaultSpan
                   Args =
                    [ Expr.Int {| Value = 2I |} |> withDefaultSpan
                      Expr.Int {| Value = 1I |} |> withDefaultSpan ] |}
            |> withDefaultSpan
            |> evalInEmptyEnv
        )
