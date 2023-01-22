module Ji.Ast

module UnaryOp =
    type T = Neg

type Expr =
    | ExprInt of int64
    | ExprUnary of op: UnaryOp.T * expr: Expr
