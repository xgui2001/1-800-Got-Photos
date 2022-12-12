module Evaluator

open Combinator
open Parser
open AST

open SixLabors.ImageSharp

// an evaluator that evaluates the AST created by the parser and matches each expression with its corresponding type
let evaluate (e: Expr) : Expr =
    match e with
    | Input f ->
        let img = Image.Load(f)
        LoadedImage(img)
    | LoadedImage (img) ->
        printfn "%A" (img.Save("Saved Image"))
        e
    | No -> failwith ("shouldnt happen")
