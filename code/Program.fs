//open Parser
//open Evaluator

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        printfn "Usage: dotnet run <filename>"
    else
        let picture = use image = Image.Load(args.[0])
        image.Save(args.[0] + ".jpg")

    0
