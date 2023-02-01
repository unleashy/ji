namespace Ji

[<RequireQualifiedAccess>]
type Value<'env> =
    | Int of bigint
    | Function of env: 'env * parameters: string list * body: SpannedExpr
