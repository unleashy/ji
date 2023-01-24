module Ji.Ast

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
    | Int of int64
    | Unary of op: UnaryOp * expr: Expr
    | Binary of left: Expr * op: BinaryOp * right: Expr
