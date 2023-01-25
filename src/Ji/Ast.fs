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
    | Function of paramNames: string list * body: Expr
    | Unary of op: UnaryOp * expr: Expr
    | Call of callee: Expr * args: Expr list
    | Binary of left: Expr * op: BinaryOp * right: Expr
