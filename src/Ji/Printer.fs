namespace Ji

module Printer =
    let print (value: Value<_>) : string =
        match value with
        | Value.Int(n) -> string n
        | Value.Function(_, parameters, _) -> $"<function/{parameters.Length}>"

    let printType (value: Value<_>) : string =
        match value with
        | Value.Int _ -> "int"
        | Value.Function(_, parameters, _) -> $"function/{parameters.Length}"

    let printUnaryOp (op: UnaryOp) : string =
        match op with
        | UnaryOp.Neg -> "`-`"

    let printBinaryOp (op: BinaryOp) : string =
        match op with
        | BinaryOp.Add -> "`+`"
        | BinaryOp.Sub -> "`-`"
        | BinaryOp.Mul -> "`*`"
        | BinaryOp.Div -> "`/`"
