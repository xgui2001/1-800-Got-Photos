open Parser
open Evaluator

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        printfn "Usage: dotnet run <filename>"
    else
        use image = Image.Load(args.[0])
        image.Save(args.[0] + ".jpg")

    match parse args.[0] with
    | Some ast -> evaluate args.[0]
    | None -> printfn "Invalid program :("

    0
