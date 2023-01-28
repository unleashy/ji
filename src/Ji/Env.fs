namespace Ji

type Env =
    private
        { bindings: Map<string, Value<Env>> }

module Env =
    let empty: Env = { bindings = Map [] }

    let get (name: string) (env: Env) : option<Value<Env>> =
        env.bindings |> Map.tryFind name

    let put (newBindings: list<string * Value<Env>>) (env: Env) : Env =
        let addBinding bindings (name, value) = Map.add name value bindings
        { bindings = newBindings |> List.fold addBinding env.bindings }
