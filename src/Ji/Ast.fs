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
           Body: Expr |}

    | Unary of {| Op: UnaryOp; Expr: Expr |}

    | Call of {| Callee: Expr; Args: Expr list |}

    | Binary of
        {| Left: Expr
           Op: BinaryOp
           Right: Expr |}
