open System
open Ji

let rep (code: string) =
    code |> Reader.read |> Evaluator.eval |> Printer.print

[<EntryPoint>]
let main _ =
    // Make sure we use UTF-8 and a neutral culture for everything
    Console.InputEncoding <- Text.Encoding.Unicode
    Console.OutputEncoding <- Text.Encoding.Unicode
    Globalization.CultureInfo.DefaultThreadCurrentCulture <-
        Globalization.CultureInfo.InvariantCulture

    printfn "• Ji repl / v0.1.0"
    printfn "Enter Ctrl+C or '\\exit' to exit\n"

    let mutable looping = true
    while looping do
        printf ">> "
        match Console.ReadLine() with
        | null
        | "\\exit" -> looping <- false
        | line ->
            let result = rep line
            printfn $"{result}"

    0
