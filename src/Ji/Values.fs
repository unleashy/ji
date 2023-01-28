namespace Ji

[<RequireQualifiedAccess>]
type Value<'env> =
    | Int of int64
    | Function of env: 'env * parameters: string list * body: Expr
