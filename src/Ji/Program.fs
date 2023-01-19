open System
open Ji

let evaluate ast = ast

let print value = $"{value}"

let rep (code: string) = Reader.read code |> evaluate |> print

[<EntryPoint>]
let main _ =
    // Make sure we use UTF-8 and a neutral culture for everything
    Console.InputEncoding <- Text.Encoding.Unicode
    Console.OutputEncoding <- Text.Encoding.Unicode
    Globalization.CultureInfo.DefaultThreadCurrentCulture <- Globalization.CultureInfo.InvariantCulture

    printfn "• Ji repl / v0.1.0"

    let mutable looping = true

    while looping do
        printf ">> "

        match Console.ReadLine() with
        | null -> looping <- false
        | line ->
            let result = rep line
            printfn $"{result}"

    0
