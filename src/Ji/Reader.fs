module Ji.Reader

open Ji.Ast

// Grammar for Ji:
//   Top → Expr <end of input>
//
//   Expr → ExprAdd
//   ExprAdd → ExprMul ([+-] ExprMul)*
//   ExprMul → ExprUnary ([*/] ExprUnary)*
//   ExprUnary → "-"? ExprCall
//
//   ExprCall → Primary ("(" Args? ")")*
//   Args → Expr ("," Expr)*
//
//   Primary → Int
//           | Function
//           | "(" Expr ")"
//
//   Int → [0-9]+
//
//   Function → "λ" Params "→" Expr
//   Params   → "(" Name* ")"
//   Name     → [A-Za-z_] [A-Za-z0-9_]*
//
//   <ignored>
//   Spacing → [\r\t ]*

let private (|SeqNil|SeqCons|) s =
    match s |> Seq.tryHead with
    | Some(head) -> SeqCons(head, s |> Seq.skip 1)
    | None -> SeqNil

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
    | Comma

module private Lexer =
    let private span f s =
        let rec loop f s acc =
            match s with
            | SeqCons(head, tail) when f head -> loop f tail (acc + string head)
            | _ -> (acc, s)

        loop f s ""

    let private isDigit c = c >= '0' && c <= '9'

    let private nextInt code =
        let text, code' = code |> span isDigit
        match text with
        | "" -> None
        | _ -> Some(Token.Int(int64 text), code')

    let private isNameStart c =
        (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_'

    let private isNameContinue c = isNameStart c || isDigit c

    let private nextName code =
        let text, code' = code |> span isNameStart
        match text with
        | "" -> None
        | _ ->
            let text', code'' = code' |> span isNameContinue
            Some(Token.Name(text + text'), code'')

    let private nextOp code =
        match code with
        | SeqCons('+', code) -> Some(Token.Plus, code)
        | SeqCons('-', code) -> Some(Token.Minus, code)
        | SeqCons('*', code) -> Some(Token.Star, code)
        | SeqCons('/', code) -> Some(Token.Slash, code)
        | SeqCons('(', code) -> Some(Token.ParenOpen, code)
        | SeqCons(')', code) -> Some(Token.ParenClose, code)
        | SeqCons('λ', code) -> Some(Token.Lambda, code)
        | SeqCons('→', code) -> Some(Token.ArrowRight, code)
        | SeqCons(',', code) -> Some(Token.Comma, code)
        | _ -> None

    let private nextFns = [ nextInt; nextName; nextOp ]

    let private isSpacing c = List.contains c [ '\r'; '\t'; ' ' ]

    let private nextToken code =
        let code = code |> Seq.skipWhile isSpacing
        let token = nextFns |> List.tryPick (fun nextFn -> nextFn code)

        match token with
        | Some(result) -> result
        | None when code |> Seq.isEmpty -> (Token.End, Seq.empty)
        | None -> failwith $"Unknown character '{Seq.head code}'"

    let tokenise (code: string) : seq<Token> =
        let unfoldInfinite f = Seq.unfold (f >> Some)
        unfoldInfinite nextToken code

let rec private readExpr tokens = readAdd tokens

and private readAdd tokens =
    let rec loop prevExpr tokens =
        let op, tokens =
            match tokens with
            | SeqCons(Token.Plus, tokens) -> (Some(BinaryOp.Add), tokens)
            | SeqCons(Token.Minus, tokens) -> (Some(BinaryOp.Sub), tokens)
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
            | SeqCons(Token.Star, tokens) -> (Some(BinaryOp.Mul), tokens)
            | SeqCons(Token.Slash, tokens) -> (Some(BinaryOp.Div), tokens)
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
    | SeqCons(Token.Minus, tokens) ->
        let expr, tokens = readCall tokens
        (Expr.Unary(UnaryOp.Neg, expr), tokens)
    | _ -> readCall tokens

and private readCall tokens =
    let rec loop prevExpr tokens =
        match readArgs tokens with
        | Some(args, tokens) -> loop (Expr.Call(prevExpr, args)) tokens
        | None -> (prevExpr, tokens)

    let left, tokens = readPrimary tokens
    loop left tokens

and private readArgs tokens =
    match tokens with
    | SeqCons(Token.ParenOpen, tokens) ->
        match tokens with
        | SeqCons(Token.ParenClose, tokens) -> Some([], tokens)
        | _ ->
            let exprs, tokens = readExprList tokens

            match tokens with
            | SeqCons(Token.ParenClose, tokens) -> Some(exprs, tokens)
            | _ -> failwith $"Expected a ')', got {tokens |> Seq.head}"
    | _ -> None

and private readExprList tokens =
    let rec loop tokens acc =
        let expr, tokens = readExpr tokens
        match tokens with
        | SeqCons(Token.Comma, tokens) -> loop tokens (acc @ [ expr ])
        | _ -> (acc @ [ expr ], tokens)

    loop tokens []

and private readPrimary tokens =
    let choice =
        [ readInt; readFunction; readParens ]
        |> List.tryPick (fun reader -> reader tokens)
    match choice with
    | Some(result) -> result
    | None -> failwith $"Unexpected {tokens |> Seq.head}"

and private readInt tokens =
    match tokens with
    | SeqCons(Token.Int(value), rest) -> Some(Expr.Int(value), rest)
    | _ -> None

and private readFunction tokens =
    match tokens with
    | SeqCons(Token.Lambda, tokens) ->
        let paramNames, tokens =
            match tokens with
            | SeqCons(Token.ParenOpen, tokens) ->
                let paramNames, tokens = readParams tokens

                match tokens with
                | SeqCons(Token.ParenClose, tokens) -> (paramNames, tokens)
                | _ -> failwith $"Expected a ')', got {tokens |> Seq.head}"
            | _ -> ([], tokens)

        match tokens with
        | SeqCons(Token.ArrowRight, tokens) ->
            let body, tokens = readExpr tokens
            Some(Expr.Function(paramNames, body), tokens)
        | _ -> failwith $"Expected an '→' for lambda, got {tokens |> Seq.head}"
    | _ -> None

and private readParams tokens =
    let rec loop tokens acc =
        match tokens with
        | SeqCons(Token.Name name, tokens) ->
            match tokens with
            | SeqCons(Token.Comma, tokens) -> loop tokens (acc @ [ name ])
            | _ -> (acc @ [ name ], tokens)
        | _ -> (acc, tokens)

    loop tokens []

and private readParens tokens =
    match tokens with
    | SeqCons(Token.ParenOpen, tokens) ->
        let expr, tokens = readExpr tokens

        match tokens with
        | SeqCons(Token.ParenClose, tokens) -> Some(expr, tokens)
        | _ -> failwith $"Expected a ')', got {tokens |> Seq.head}"
    | _ -> None

let read (code: string) : Expr =
    let ast, tokens = readExpr (Lexer.tokenise code)
    match tokens |> Seq.head with
    | Token.End -> ast
    | extra -> failwith $"Extraneous input: unexpected {extra}"
