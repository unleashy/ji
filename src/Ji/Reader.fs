module Ji.Reader

open Ji.Ast

// Grammar for Ji:
//   Top → Expr <end of input>
//   Expr → Primary
//   Primary → Int
//   Int → [0-9]+
//
//   <ignored>
//   Spacing → [\r\t ]*

module private Token =
    type T =
        | End
        | Int of int64

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

    let private isSpacing c = List.contains c [ '\r'; '\t'; ' ' ]

    let private nextToken code =
        let code' = code |> Seq.skipWhile isSpacing
        match nextInt code' with
        | Some(result) -> result
        | None when code' |> Seq.isEmpty -> (Token.End, Seq.empty)
        | None -> failwith $"Unknown character '{Seq.head code'}'"

    let tokenise (code: string) : seq<Token.T> =
        let unfoldInfinite f = Seq.unfold (f >> Some)
        unfoldInfinite nextToken code

let private readInt tokens =
    match tokens |> Seq.head with
    | Token.Int(value) -> Some(ExprInt(value), tokens |> Seq.skip 1)
    | _ -> None

let private readPrimary tokens =
    match readInt tokens with
    | Some(result) -> result
    | None -> failwith $"Unexpected {tokens |> Seq.head}"

let private readExpr tokens = readPrimary tokens

let read (code: string) : Expr =
    let ast, tokens = readExpr (Lexer.tokenise code)
    match tokens |> Seq.head with
    | Token.End -> ast
    | extra -> failwith $"Extraneous input: unexpected {extra}"
