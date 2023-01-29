namespace Ji

type ErrorCode =
    | UnknownChar = 1
    | ExtraneousInput = 2

exception JiError of
    {| Code: ErrorCode
       Message: string
       Location: Location |}

module Error =
    let formatErrorCode (code: ErrorCode) : string =
        "JI" + (code |> int |> string).PadLeft(3, '0')
