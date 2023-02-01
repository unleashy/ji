namespace Ji

[<RequireQualifiedAccess>]
type UnaryOp = Neg

[<RequireQualifiedAccess>]
type BinaryOp =
    | Add
    | Sub
    | Mul
    | Div

[<RequireQualifiedAccess>]
type Expr =
    | Int of {| Value: bigint |}

    | Name of {| Value: string |}

    | Function of
        {| Parameters: string list
           Body: SpannedExpr |}

    | Unary of {| Op: UnaryOp; Expr: SpannedExpr |}

    | Call of
        {| Callee: SpannedExpr
           Args: SpannedExpr list |}

    | Binary of
        {| Left: SpannedExpr
           Op: BinaryOp
           Right: SpannedExpr |}

and SpannedExpr = { Expr: Expr; Span: Span }
