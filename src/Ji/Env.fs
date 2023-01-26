module Ji.Env

open Ji.Values

type T =
    private { bindings: Map<string, Value> }

let empty: T = { bindings = Map [] }
