module Ji.Printer

open Ji.Values

let print (value: Value<_>) : string =
    match value with
    | Value.Int(n) -> string n
    | Value.Function _ -> "<function>"
