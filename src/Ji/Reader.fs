module Ji.Reader

open Ji.Ast

// Grammar for Ji:
//   Expr → Primary
//   Primary → Int
//   Int → [0-9]+

let private readInt code = ExprInt(int code)

let private readPrimary code = readInt code

let private readExpr code = readPrimary code

let read (code: string) : Expr = readExpr code
