module AST

open SixLabors.ImageSharp

// Constructors for Expr Type
type Expr =
    | Input of filename: string // open a file
    | Info of filename: string
    | LoadedImage of Image
    | Output of filename: string
    | Variable of string
    | Assignment of Expr * Expr
    | Sequence of Expr list
    | No
