module Ji.Env

open Ji.Values

type T =
    private
        { bindings: Map<string, Value<T>> }

let empty: T = { bindings = Map [] }

let get (name: string) (env: T) : option<Value<T>> =
    env.bindings |> Map.tryFind name

let put (newBindings: list<string * Value<T>>) (env: T) : T =
    let addBinding bindings (name, value) = Map.add name value bindings
    { bindings = newBindings |> List.fold addBinding env.bindings }
