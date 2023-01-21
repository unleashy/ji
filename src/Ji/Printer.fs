module Ji.Printer

open Ji.Value

let print (value: Value) : string =
    match value with
    | ValueInt(n) -> string n
