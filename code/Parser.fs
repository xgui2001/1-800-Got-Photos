module Parser

open System

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing

type Expr =
    | Variable of Image
    | Application of Image * Expr

let expr, exprImpl = recparser ()

let variable: Parser<Expr>
// an implementation for the expr parser that determines whether to use the variable parser, the abstraction parser or the application parser.
exprImpl := (variable) <|> (application)