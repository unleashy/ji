module Ji.Ast

module UnaryOp =
    type T = Neg

module BinaryOp =
    type T =
        | Add
        | Sub
        | Mul
        | Div

type Expr =
    | ExprInt of int64
    | ExprUnary of op: UnaryOp.T * expr: Expr
    | ExprBinary of left: Expr * op: BinaryOp.T * right: Expr
