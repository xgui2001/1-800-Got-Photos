module AST

open SixLabors.ImageSharp

// Constructors for Expr Type
type Expr =
    | Input of filename: string
    | LoadedImage of Image
    | No
