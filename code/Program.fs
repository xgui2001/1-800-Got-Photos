open AST
open System
open Parser

open Combinator

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
        | Success (ast, _) -> printfn "%A" ast
        | Failure (_, _) -> printfn "Didnt work"

        0
