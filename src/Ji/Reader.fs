module Ji.Reader

open Ji.Ast

// Grammar for Ji:
//   Top → Expr <end of input>
//
//   Expr → ExprAdd
//   ExprAdd → ExprMul ([+-] ExprMul)*
//   ExprMul → ExprUnary ([*/] ExprUnary)*
//   ExprUnary → "-"? Primary
//
//   Primary → Int
//           / "(" Expr ")"
//   Int → [0-9]+
//
//   <ignored>
//   Spacing → [\r\t ]*

let private (|SeqNil|SeqCons|) s =
    match s |> Seq.tryHead with
    | Some(head) -> SeqCons(head, s |> Seq.skip 1)
    | None -> SeqNil

module private Token =
    type T =
        | End
        | Int of int64
        | Plus
        | Minus
        | Star
        | Slash
        | ParenOpen
        | ParenClose

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

    let private nextOp code =
        match code with
        | SeqCons('+', code) -> Some(Token.Plus, code)
        | SeqCons('-', code) -> Some(Token.Minus, code)
        | SeqCons('*', code) -> Some(Token.Star, code)
        | SeqCons('/', code) -> Some(Token.Slash, code)
        | SeqCons('(', code) -> Some(Token.ParenOpen, code)
        | SeqCons(')', code) -> Some(Token.ParenClose, code)
        | _ -> None

    let private nextFns = [ nextInt; nextOp ]

    let private isSpacing c = List.contains c [ '\r'; '\t'; ' ' ]

    let private nextToken code =
        let code = code |> Seq.skipWhile isSpacing
        let token = nextFns |> List.tryPick (fun nextFn -> nextFn code)

        match token with
        | Some(result) -> result
        | None when code |> Seq.isEmpty -> (Token.End, Seq.empty)
        | None -> failwith $"Unknown character '{Seq.head code}'"

    let tokenise (code: string) : seq<Token.T> =
        let unfoldInfinite f = Seq.unfold (f >> Some)
        unfoldInfinite nextToken code

let rec private readExpr tokens = readAdd tokens

and private readAdd tokens =
    let (left, tokens) = readMul tokens

    let rec loop prevExpr tokens =
        let op, tokens =
            match tokens with
            | SeqCons(Token.Plus, tokens) -> (Some(BinaryOp.Add), tokens)
            | SeqCons(Token.Minus, tokens) -> (Some(BinaryOp.Sub), tokens)
            | _ -> (None, tokens)

        match op with
        | Some(op) ->
            let right, tokens = readMul tokens
            loop (ExprBinary(prevExpr, op, right)) tokens
        | None -> (prevExpr, tokens)

    loop left tokens

and private readMul tokens =
    let (left, tokens) = readUnary tokens

    let rec loop prevExpr tokens =
        let op, tokens =
            match tokens with
            | SeqCons(Token.Star, tokens) -> (Some(BinaryOp.Mul), tokens)
            | SeqCons(Token.Slash, tokens) -> (Some(BinaryOp.Div), tokens)
            | _ -> (None, tokens)

        match op with
        | Some(op) ->
            let right, tokens = readUnary tokens
            loop (ExprBinary(prevExpr, op, right)) tokens
        | None -> (prevExpr, tokens)

    loop left tokens

and private readUnary tokens =
    match tokens with
    | SeqCons(Token.Minus, tokens) ->
        let expr, tokens = readPrimary tokens
        (ExprUnary(UnaryOp.Neg, expr), tokens)
    | _ -> readPrimary tokens

and private readPrimary tokens =
    let choice =
        [ readInt; readParens ] |> List.tryPick (fun reader -> reader tokens)
    match choice with
    | Some(result) -> result
    | None -> failwith $"Unexpected {tokens |> Seq.head}"

and private readInt tokens =
    match tokens with
    | SeqCons(Token.Int(value), rest) -> Some(ExprInt(value), rest)
    | _ -> None

and private readParens tokens =
    match tokens with
    | SeqCons(Token.ParenOpen, tokens) ->
        let expr, tokens = readExpr tokens

        match tokens with
        | SeqCons(Token.ParenClose, tokens) -> Some(expr, tokens)
        | _ -> failwith $"Expected a ParenClose, got {tokens |> Seq.head}"
    | _ -> None

let read (code: string) : Expr =
    let ast, tokens = readExpr (Lexer.tokenise code)
    match tokens |> Seq.head with
    | Token.End -> ast
    | extra -> failwith $"Extraneous input: unexpected {extra}"
