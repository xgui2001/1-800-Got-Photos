module Parser

open System

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing

// constructs an Expr type; it could be a Variable of Image type (for now)
type Expr =
    | Variable of Image
    //| Application of Image * Expr <-- will implement later

// a declaration for a parser called expr and an implementation for that same parser called exprImpl.
// takes in an Input type and returns Outcome <Expr> type.
let expr, exprImpl = recparser ()

// a declaration for a variable parser that parses in a char and returns a Variable. Return type is of Parser<Expr>
let variable: Parser<Expr> = use image.Load(Cat03.jpeg) //(fun a:Image -> Variable (a))

let parse input : Expr option =
    match grammar (prepare input) with
    | Success (ws, _) -> Some ws
    | Failure (_, _) -> None


// a declaration for a grammar parser that takes in an expr and checks whether it has reached the end of the input. Return type is of Parser<Expr>
let grammar = pleft expr peof

// an implementation for the expr parser that determines whether to use the variable parser, the abstraction parser or the application parser.
exprImpl := (variable) <|> (application)