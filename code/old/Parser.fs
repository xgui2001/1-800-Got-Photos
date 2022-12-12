module Parser

open Combinator
open System

//open SixLabors.ImageSharp
//open SixLabors.ImageSharp.Drawing

// constructs an Expr type;
type Expr =
    | ImageName of String
    | FunctionName of String
    | Application of Expr * Expr //FunctionName * ImageName

// a declaration for a parser called expr and an implementation for that same parser called exprImpl.
// takes in an Input type and returns Outcome <Expr> type.
let expr, exprImpl = recparser ()

// a declaration for an image parser that parses in a filename and returns a ImageName. Return type is of Parser<Expr>
let imagename: Parser<Expr> = (pmany1 pletter |>> (fun a -> ImageName(stringify a)))

// a declaration for an image parser that parses in a filename and returns a ImageName. Return type is of Parser<Expr>
let functionname: Parser<Expr> =
    (pmany1 pletter |>> (fun a -> FunctionName(stringify a)))

// a declaration for an application parser that parses in an expr and an imagename nd combine them into an Application. Return type is of Parser<Expr>
let application: Parser<Expr> =
    pseq (functionname) (pbetween (pchar '(') (pchar ')') (imagename)) (fun (a, b) -> Application(a, b))

// a parser that parses in an image with our grammar parser; if it was successfully parsed, return an Expr.
// otherwise return none
let parse input : Expr option =
    match input with
    | Success (ws, _) -> Some ws
    | Failure (_, _) -> None

// a declaration for a grammar parser that takes in an expr and checks whether it has reached the end of the input. Return type is of Parser<Expr>
let grammar = pleft expr peof

// an implementation for the expr parser that determines whether to use the variable parser, the abstraction parser or the application parser.
exprImpl := (imagename) <|> (functionname) <|> (application)
