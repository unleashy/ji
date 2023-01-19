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

let private isEmpty code = String.length code = 0
let private isNotEmpty = isEmpty >> not

let private testFst f code = code |> isNotEmpty && f (code[0])

let private takeWhile f code =
    let rec impl code index =
        if code |> testFst f then
            impl code[1..] (index + 1)
        else
            index

    let cutoff = impl code 0
    (code[0 .. cutoff - 1], code[cutoff..])

let private skipWhile f code = code |> takeWhile f |> snd

let private skipSpacing =
    skipWhile (fun c -> List.contains c [ '\r'; '\t'; ' ' ])

let private isDigit c = c >= '0' && c <= '9'

let private readInt code =
    if code |> testFst isDigit then
        let intText, code = code |> takeWhile isDigit
        Some(ExprInt(uint64 intText), code)
    else
        None

let private readPrimary code = Option.get (readInt code)

let private readExpr code = readPrimary code

let read (code: string) : Expr =
    let ast, code = code |> skipSpacing |> readExpr
    if code |> skipSpacing |> isEmpty then
        ast
    else
        failwith "Extraneous input"
