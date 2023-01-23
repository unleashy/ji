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
            match s |> Seq.tryHead with
            | Some(head) when f head ->
                let tail = s |> Seq.skip 1
                loop f tail (acc + string head)
            | _ -> (acc, s)

        loop f s ""

    let private isDigit c = c >= '0' && c <= '9'

    let private nextInt code =
        let text, code' = code |> span isDigit
        match text with
        | "" -> None
        | _ -> Some(Token.Int(int64 text), code')

    let private nextOp code =
        code
        |> Seq.tryHead
        |> Option.bind (function
            | '+' -> Some(Token.Plus, code |> Seq.skip 1)
            | '-' -> Some(Token.Minus, code |> Seq.skip 1)
            | '*' -> Some(Token.Star, code |> Seq.skip 1)
            | '/' -> Some(Token.Slash, code |> Seq.skip 1)
            | '(' -> Some(Token.ParenOpen, code |> Seq.skip 1)
            | ')' -> Some(Token.ParenClose, code |> Seq.skip 1)
            | _ -> None)

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

    let op =
        match tokens |> Seq.head with
        | Token.Plus -> Some(BinaryOp.Add)
        | Token.Minus -> Some(BinaryOp.Sub)
        | _ -> None

    match op with
    | Some(op) ->
        let right, tokens = readAdd (tokens |> Seq.skip 1)
        (ExprBinary(left, op, right), tokens)
    | None -> (left, tokens)

and private readMul tokens =
    let (left, tokens) = readUnary tokens

    let op =
        match tokens |> Seq.head with
        | Token.Star -> Some(BinaryOp.Mul)
        | Token.Slash -> Some(BinaryOp.Div)
        | _ -> None

    match op with
    | Some(op) ->
        let right, tokens = readMul (tokens |> Seq.skip 1)
        (ExprBinary(left, op, right), tokens)
    | None -> (left, tokens)

and private readUnary tokens =
    match tokens |> Seq.head with
    | Token.Minus ->
        let expr, tokens = readPrimary (tokens |> Seq.skip 1)
        (ExprUnary(UnaryOp.Neg, expr), tokens)
    | _ -> readPrimary tokens

and private readPrimary tokens =
    let choice =
        [ readInt; readParens ] |> List.tryPick (fun reader -> reader tokens)
    match choice with
    | Some(result) -> result
    | None -> failwith $"Unexpected {tokens |> Seq.head}"

and private readInt tokens =
    match tokens |> Seq.head with
    | Token.Int(value) -> Some(ExprInt(value), tokens |> Seq.skip 1)
    | _ -> None

and private readParens tokens =
    match tokens |> Seq.head with
    | Token.ParenOpen ->
        let expr, tokens = readExpr (tokens |> Seq.skip 1)

        match tokens |> Seq.head with
        | Token.ParenClose -> Some(expr, tokens |> Seq.skip 1)
        | token -> failwith $"Expected a ParenClose, got {token}"
    | _ -> None

let read (code: string) : Expr =
    let ast, tokens = readExpr (Lexer.tokenise code)
    match tokens |> Seq.head with
    | Token.End -> ast
    | extra -> failwith $"Extraneous input: unexpected {extra}"
