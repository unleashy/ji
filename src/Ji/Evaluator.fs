module Ji.Evaluator

open Ji.Ast
open Ji.Value

let eval (expr: Expr) : Value =
    match expr with
    | ExprInt(n) -> ValueInt(n)
    | _ -> failwith $"Unknown AST {expr}"
