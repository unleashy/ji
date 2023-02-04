namespace Ji

type ErrorCode =
    | UnknownChar = 1
    | ExtraneousInput = 2
    | ExpectedExpr = 3
    | ExpectedArrow = 4
    | UnclosedParens = 5
    | NotInScope = 6
    | InvalidCall = 7
    | InvalidType = 8
    | InvalidTypeUnary = 9
    | InvalidTypeBinary = 10

type JiException(message: string, code: ErrorCode, location: Location) =
    inherit System.Exception(message)

    member _.Code = code
    member _.Location = location

module Error =
    type RaiseWithRecord =
        { Code: ErrorCode
          Message: string
          Span: Span }

    let raiseWith (data: RaiseWithRecord) =
        raise <| JiException(data.Message, data.Code, Location.ofSpan data.Span)

    let formatErrorCode (code: ErrorCode) : string =
        "JI" + (code |> int |> string).PadLeft(3, '0')
