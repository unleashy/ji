namespace Ji

open Ji.Error

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
//   Spacing → [\r\t ]*

[<RequireQualifiedAccess>]
type private Token =
    | End
    | Int of int64
    | Name of string
    | Plus
    | Minus
    | Star
    | Slash
    | ParenOpen
    | ParenClose
    | Lambda
    | ArrowRight

type private LocatedToken =
    { Token: Token
      Location: Lazy<Location> }

module private Lexer =
    type private Source = { Code: string; CurrentIndex: int }

    module private Source =
        let make (code: string) : Source = { Code = code; CurrentIndex = 0 }

        let isEmpty (s: Source) : bool = s.CurrentIndex >= s.Code.Length

        let head (s: Source) : char = s.Code[s.CurrentIndex]

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
                Some { s with CurrentIndex = s.CurrentIndex + needle.Length }
            else
                None

    let private isDigit c = c >= '0' && c <= '9'

    let private nextInt source =
        let text, source = source |> Source.consumeWhile isDigit
        match text with
        | "" -> None
        | _ -> Some(Token.Int(int64 text), source)

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
        | Source.Match "+" rest -> Some(Token.Plus, rest)
        | Source.Match "-" rest -> Some(Token.Minus, rest)
        | Source.Match "*" rest -> Some(Token.Star, rest)
        | Source.Match "/" rest -> Some(Token.Slash, rest)
        | Source.Match "(" rest -> Some(Token.ParenOpen, rest)
        | Source.Match ")" rest -> Some(Token.ParenClose, rest)
        | Source.Match "λ" rest -> Some(Token.Lambda, rest)
        | Source.Match "→" rest -> Some(Token.ArrowRight, rest)
        | _ -> None

    let private nextFns = [ nextInt; nextName; nextOp ]

    let private isSpacing c = List.contains c [ '\r'; '\t'; ' ' ]

    let private nextToken source =
        let source = source |> Source.skipWhile isSpacing
        let token = nextFns |> List.tryPick (fun nextFn -> nextFn source)
        let location = lazy (Location.fromIndex source.Code source.CurrentIndex)

        match token with
        | Some(token, source') ->
            ({ Token = token; Location = location }, source')

        | None when source |> Source.isEmpty ->
            ({ Token = Token.End
               Location = location },
             source)

        | None -> failwith $"Unknown character '{source |> Source.head}'"

    let tokenise (code: string) : seq<LocatedToken> =
        let unfoldInfinite f = Seq.unfold (f >> Some)
        unfoldInfinite nextToken (Source.make code)

module Reader =
    let private (|SeqNil|SeqCons|) s =
        match s |> Seq.tryHead with
        | Some(head) -> SeqCons(head, s |> Seq.skip 1)
        | None -> SeqNil

    let rec private readExpr tokens = readAdd tokens

    and private readAdd tokens =
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
                loop (Expr.Binary(prevExpr, op, right)) tokens
            | None -> (prevExpr, tokens)

        let left, tokens = readMul tokens
        loop left tokens

    and private readMul tokens =
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
                loop (Expr.Binary(prevExpr, op, right)) tokens
            | None -> (prevExpr, tokens)

        let left, tokens = readUnary tokens
        loop left tokens

    and private readUnary tokens =
        match tokens with
        | SeqCons({ Token = Token.Minus }, tokens) ->
            let expr, tokens = readCall tokens
            (Expr.Unary(UnaryOp.Neg, expr), tokens)
        | _ -> readCall tokens

    and private readCall tokens =
        let isPrimaryStart ({ Token = token }) =
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
            (Expr.Call(callee, args), tokens)
        else
            (callee, tokens)

    and private readPrimary tokens =
        let choice =
            [ readInt; readName; readFunction; readParens ]
            |> List.tryPick (fun reader -> reader tokens)
        match choice with
        | Some(result) -> result
        | None -> failwith $"Unexpected {tokens |> Seq.head}"

    and private readInt tokens =
        match tokens with
        | SeqCons({ Token = Token.Int(value) }, rest) ->
            Some(Expr.Int(value), rest)
        | _ -> None

    and private readName tokens =
        match tokens with
        | SeqCons({ Token = Token.Name(value) }, rest) ->
            Some(Expr.Name(value), rest)
        | _ -> None

    and private readFunction tokens =
        match tokens with
        | SeqCons({ Token = Token.Lambda }, tokens) ->
            let paramNames, tokens = readParams tokens

            match tokens with
            | SeqCons({ Token = Token.ArrowRight }, tokens) ->
                let body, tokens = readExpr tokens
                Some(Expr.Function(paramNames, body), tokens)
            | _ ->
                failwith $"Expected an '→' for lambda, got {tokens |> Seq.head}"
        | _ -> None

    and private readParams tokens =
        let rec loop tokens acc =
            match tokens with
            | SeqCons({ Token = Token.Name name }, tokens) ->
                loop tokens (acc @ [ name ])
            | _ -> (acc, tokens)

        loop tokens []

    and private readParens tokens =
        match tokens with
        | SeqCons({ Token = Token.ParenOpen }, tokens) ->
            let expr, tokens = readExpr tokens

            match tokens with
            | SeqCons({ Token = Token.ParenClose }, tokens) ->
                Some(expr, tokens)
            | _ -> failwith $"Expected a ')', got {tokens |> Seq.head}"
        | _ -> None

    let read (code: string) : Expr =
        let ast, tokens = readExpr (Lexer.tokenise code)
        match tokens |> Seq.head with
        | { Token = Token.End } -> ast
        | { Token = extra } -> failwith $"Extraneous input: unexpected {extra}"
