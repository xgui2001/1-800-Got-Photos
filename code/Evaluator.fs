module Evaluator

open Parser

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing

// an evaluator that evaluates the AST created by the parser and matches each expression with its corresponding type
let evaluate =
    match expr with
    | Variable -> image.Save()
//  | Application ->
