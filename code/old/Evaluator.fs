module Evaluator

open Parser

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing

// an evaluator that evaluates the AST created by the parser and matches each expression with its corresponding type
let evaluate (e: Expr) : string =
    match e with
    | ImageName i -> "Variable(" + i.ToString() + ")"
    | FunctionName f -> "FunctionName(" + f.ToString() + ")"
    | Application (f, i) -> "TODO"
