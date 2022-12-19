open AST
open System
open Parser

open Combinator
open Evaluator

// a main method to parse the evaluated input with our parsers; if it was successfully parsed, print the ast out
// otherwise tells the user the program input is invalid
[<EntryPoint>]
let main args =
    if args.Length < 1 then
        printfn "Usage: dotnet run <filename>"
        1
    else
        let path = args.[0]
        let program = IO.File.ReadAllText(path)
        let input = debug program

        match grammar input with
        | Success (ast, _) ->
            try
                let ev = eval ast Map.empty
                //printfn "%A" ev
                printfn "Program Success!"
            with :? Exception as e ->
                printfn "Program failure: %s" e.Message
        | Failure (_, _) -> printfn "Invalid input :("

        0
