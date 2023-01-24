module Ji.Printer

open Ji.Values

let print (value: Value) : string =
    match value with
    | Value.Int(n) -> string n
