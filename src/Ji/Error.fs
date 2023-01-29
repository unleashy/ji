namespace Ji

type ErrorCode =
    | UnknownChar = 1
    | ExtraneousInput = 2
    | ExpectedExpr = 3
    | ExpectedArrow = 4
    | UnclosedParens = 5

exception JiError of
    {| Code: ErrorCode
       Message: string
       Location: Location |}

module Error =
    let raiseWith data = raise <| JiError data

    let formatErrorCode (code: ErrorCode) : string =
        "JI" + (code |> int |> string).PadLeft(3, '0')
