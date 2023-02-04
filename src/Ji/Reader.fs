namespace Ji

// Grammar for Ji:
//   Top → Expr <end of input>
//
//   Expr → ExprAdd
//   ExprAdd → ExprMul ([+-] ExprMul)*
//   ExprMul → ExprUnary ([*/] ExprUnary)*
//   ExprUnary → "-"? ExprCall
//   ExprCall → Primary Primary*
//
//   Primary → Int
//           | Name
//           | Function
//           | "(" Expr ")"
//
//   Int → [0-9]+
//
//   Name → [A-Za-z_] [A-Za-z0-9_]*
//
//   Function → "λ" Name* "→" Expr
//
//   <ignored>
//   Spacing → (Comment | [\r\t ])*
//   Comment → "#" <any>* ("\n" | <end of input>)

[<RequireQualifiedAccess>]
type private Token =
    | End
    | Int of bigint
    | Name of string
    | Plus
    | Minus
    | Star
    | Slash
    | ParenOpen
    | ParenClose
    | Lambda
    | ArrowRight

    override this.ToString() : string =
        match this with
        | End -> "end of input"
        | Int i -> string i
        | Name n -> n
        | Plus -> "'+'"
        | Minus -> "'-'"
        | Star -> "'*'"
        | Slash -> "'/'"
        | ParenOpen -> "'('"
        | ParenClose -> "')'"
        | Lambda -> "'λ' or '\\'"
        | ArrowRight -> "'→' or '->'"

type private SpannedToken = { Token: Token; Span: Span }

module private Lexer =
    type private Source = { Code: string; CurrentIndex: int }

    module private Source =
        let make (code: string) : Source = { Code = code; CurrentIndex = 0 }

        let isEmpty (s: Source) : bool = s.CurrentIndex >= s.Code.Length

        let head (s: Source) : char = s.Code[s.CurrentIndex]

        let skip (n: int) (s: Source) : Source =
            { s with CurrentIndex = s.CurrentIndex + n }

        let skipWhile (f: char -> bool) (s: Source) : Source =
            let rec loop f index =
                match s.Code[index..] |> Seq.tryHead with
                | Some c when f c -> loop f (index + 1)
                | _ -> index

            let index = loop f s.CurrentIndex
            { s with CurrentIndex = index }

        let consumeWhile (f: char -> bool) (s: Source) : string * Source =
            let rec loop f index acc =
                match s.Code[index..] |> Seq.tryHead with
                | Some c when f c -> loop f (index + 1) (acc + string c)
                | _ -> (acc, index)

            let result, index = loop f s.CurrentIndex ""
            (result, { s with CurrentIndex = index })

        let test (str: string) (s: Source) : bool =
            s.Code[s.CurrentIndex .. s.CurrentIndex + str.Length - 1] = str

        let (|Match|_|) (needle: string) (s: Source) =
            if s |> test needle then
                Some(s |> skip needle.Length)
            else
                None

    let rec private skipComment source =
        source |> Source.skipWhile (fun c -> c <> '\n') |> Source.skip 1

    let rec private skipSpacing source =
        match source with
        | Source.Match "\r" rest
        | Source.Match "\t" rest
        | Source.Match " " rest -> skipSpacing rest
        | Source.Match "#" rest -> skipComment rest
        | _ -> source

    let private isDigit c = c >= '0' && c <= '9'

    let private nextInt source =
        let text, source = source |> Source.consumeWhile isDigit
        match text with
        | "" -> None
        | _ -> Some(Token.Int(System.Numerics.BigInteger.Parse(text)), source)

    let private isNameStart c =
        (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_'

    let private isNameContinue c = isNameStart c || isDigit c

    let private nextName source =
        let text, source = source |> Source.consumeWhile isNameStart
        match text with
        | "" -> None
        | _ ->
            let text', source = source |> Source.consumeWhile isNameContinue
            Some(Token.Name(text + text'), source)

    let private nextOp source =
        match source with
        | Source.Match "->" rest
        | Source.Match "→" rest -> Some(Token.ArrowRight, rest)
        | Source.Match "+" rest -> Some(Token.Plus, rest)
        | Source.Match "-" rest -> Some(Token.Minus, rest)
        | Source.Match "*" rest -> Some(Token.Star, rest)
        | Source.Match "/" rest -> Some(Token.Slash, rest)
        | Source.Match "(" rest -> Some(Token.ParenOpen, rest)
        | Source.Match ")" rest -> Some(Token.ParenClose, rest)
        | Source.Match "λ" rest
        | Source.Match @"\" rest -> Some(Token.Lambda, rest)
        | _ -> None

    let private nextFns = [ nextInt; nextName; nextOp ]

    let private nextToken source =
        let source = source |> skipSpacing
        let token = nextFns |> List.tryPick (fun nextFn -> nextFn source)

        match token with
        | Some(token, source') ->
            ({ Token = token
               Span =
                 Span.ofSlice
                     source.Code
                     source.CurrentIndex
                     source'.CurrentIndex },
             source')

        | None when source |> Source.isEmpty ->
            ({ Token = Token.End
               Span = Span.emptyAt source.Code source.CurrentIndex },
             source)

        | None ->
            let unknownChar = source |> Source.head
            Error.raiseWith
                { Code = ErrorCode.UnknownChar
                  Message = $"Unknown character {unknownChar}"
                  Span =
                    { Code = source.Code
                      Index = source.CurrentIndex
                      Length = 1 } }

    let tokenise (code: string) : seq<SpannedToken> =
        let unfoldInfinite f = Seq.unfold (f >> Some)
        unfoldInfinite nextToken (Source.make code)

module Reader =
    let private (|SeqNil|SeqCons|) s =
        match s |> Seq.tryHead with
        | Some(head) -> SeqCons(head, s |> Seq.skip 1)
        | None -> SeqNil

    let private readImpl =
        let rec readExpr tokens = readAdd tokens

        and readAdd tokens =
            let rec loop prevExpr tokens =
                let op, tokens =
                    match tokens with
                    | SeqCons({ Token = Token.Plus }, tokens) ->
                        (Some(BinaryOp.Add), tokens)

                    | SeqCons({ Token = Token.Minus }, tokens) ->
                        (Some(BinaryOp.Sub), tokens)

                    | _ -> (None, tokens)

                match op with
                | Some(op) ->
                    let right, tokens = readMul tokens
                    let expr =
                        { Expr =
                            Expr.Binary
                                {| Left = prevExpr
                                   Op = op
                                   Right = right |}
                          Span = prevExpr.Span ++ right.Span }

                    loop expr tokens
                | None -> (prevExpr, tokens)

            let left, tokens = readMul tokens
            loop left tokens

        and readMul tokens =
            let rec loop prevExpr tokens =
                let op, tokens =
                    match tokens with
                    | SeqCons({ Token = Token.Star }, tokens) ->
                        (Some(BinaryOp.Mul), tokens)

                    | SeqCons({ Token = Token.Slash }, tokens) ->
                        (Some(BinaryOp.Div), tokens)

                    | _ -> (None, tokens)

                match op with
                | Some(op) ->
                    let right, tokens = readUnary tokens
                    let expr =
                        { Expr =
                            Expr.Binary
                                {| Left = prevExpr
                                   Op = op
                                   Right = right |}
                          Span = prevExpr.Span ++ right.Span }

                    loop expr tokens
                | None -> (prevExpr, tokens)

            let left, tokens = readUnary tokens
            loop left tokens

        and readUnary tokens =
            match tokens with
            | SeqCons({ Token = Token.Minus
                        Span = minusSpan },
                      tokens) ->
                let expr, tokens = readCall tokens

                ({ Expr = Expr.Unary {| Op = UnaryOp.Neg; Expr = expr |}
                   Span = minusSpan ++ expr.Span },
                 tokens)
            | _ -> readCall tokens

        and readCall tokens =
            let isPrimaryStart { Token = token } =
                match token with
                | Token.Int _
                | Token.Name _
                | Token.ParenOpen _ -> true
                | _ -> false

            let rec loop args tokens =
                if tokens |> Seq.head |> isPrimaryStart then
                    let arg, tokens = readPrimary tokens
                    loop (args @ [ arg ]) tokens
                else
                    (args, tokens)

            let callee, tokens = readPrimary tokens
            if tokens |> Seq.head |> isPrimaryStart then
                let args, tokens = loop [] tokens
                let span = callee.Span ++ (args |> List.last).Span

                let expr =
                    { Expr = Expr.Call {| Callee = callee; Args = args |}
                      Span = span }

                (expr, tokens)
            else
                (callee, tokens)

        and readPrimary tokens : SpannedExpr * seq<SpannedToken> =
            let choice =
                [ readInt; readName; readFunction; readParens ]
                |> List.tryPick (fun reader -> reader tokens)
            match choice with
            | Some(result) -> result
            | None ->
                let token = tokens |> Seq.head
                Error.raiseWith
                    { Code = ErrorCode.ExpectedExpr
                      Message = $"Expected an expression but got {token.Token}"
                      Span = token.Span }

        and readInt tokens =
            match tokens with
            | SeqCons({ Token = Token.Int(value)
                        Span = span },
                      rest) ->
                let expr =
                    { Expr = Expr.Int {| Value = value |}
                      Span = span }

                Some(expr, rest)
            | _ -> None

        and readName tokens =
            match tokens with
            | SeqCons({ Token = Token.Name(value)
                        Span = span },
                      rest) ->
                let expr =
                    { Expr = Expr.Name {| Value = value |}
                      Span = span }

                Some(expr, rest)
            | _ -> None

        and readFunction tokens =
            match tokens with
            | SeqCons({ Token = Token.Lambda
                        Span = lambdaSpan },
                      tokens) ->
                let paramNames, tokens = readParams tokens

                match tokens with
                | SeqCons({ Token = Token.ArrowRight }, tokens) ->
                    let body, tokens = readExpr tokens
                    let expr =
                        { Expr =
                            Expr.Function
                                {| Parameters = paramNames
                                   Body = body |}
                          Span = lambdaSpan ++ body.Span }

                    Some(expr, tokens)
                | _ ->
                    let token = tokens |> Seq.head
                    Error.raiseWith
                        { Code = ErrorCode.ExpectedArrow
                          Message =
                            $"Expected an '→' or '->' to continue lambda, got {token.Token}"
                          Span = token.Span }
            | _ -> None

        and readParams tokens =
            let rec loop tokens acc =
                match tokens with
                | SeqCons({ Token = Token.Name name }, tokens) ->
                    loop tokens (acc @ [ name ])
                | _ -> (acc, tokens)

            loop tokens []

        and readParens tokens =
            match tokens with
            | SeqCons({ Token = Token.ParenOpen
                        Span = openSpan },
                      tokens) ->
                let expr, tokens = readExpr tokens

                match tokens with
                | SeqCons({ Token = Token.ParenClose
                            Span = closeSpan },
                          tokens) ->
                    Some({ expr with Span = openSpan ++ closeSpan }, tokens)
                | _ ->
                    let token = tokens |> Seq.head
                    let startLocation = Location.ofSpan openSpan
                    Error.raiseWith
                        { Code = ErrorCode.UnclosedParens
                          Message =
                            $"Unclosed parenthesised expression starting at {startLocation}"
                          Span = token.Span }
            | _ -> None

        readExpr

    let read (code: string) : SpannedExpr =
        let ast, tokens = readImpl (Lexer.tokenise code)
        match tokens |> Seq.head with
        | { Token = Token.End } -> ast
        | { Token = extra; Span = span } ->
            Error.raiseWith
                { Code = ErrorCode.ExtraneousInput
                  Message = $"Extraneous input: unexpected {extra}"
                  Span = span }
