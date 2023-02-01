namespace Ji

type ErrorCode =
    | UnknownChar = 1
    | ExtraneousInput = 2
    | ExpectedExpr = 3
    | ExpectedArrow = 4
    | UnclosedParens = 5

type JiException(message: string, code: ErrorCode, location: Location) =
    inherit System.Exception(message)

    member _.Code = code
    member _.Location = location

module Error =
    type RaiseWithRecord =
        {| Code: ErrorCode
           Message: string
           Location: Location |}

    let raiseWith (data: RaiseWithRecord) =
        raise <| JiException(data.Message, data.Code, data.Location)

    let formatErrorCode (code: ErrorCode) : string =
        "JI" + (code |> int |> string).PadLeft(3, '0')
