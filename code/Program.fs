open Parser
open Evaluator

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing

[<EntryPoint>]
let main args =
if args.Length < int args.[0] then
    printfn "Usage: dotnet run <filename>"
  else