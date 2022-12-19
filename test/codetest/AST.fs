module AST

open SixLabors.ImageSharp

// Constructors for Expr
type Expr =
    | LoadedImage of Image
    | Input of filename: string
    | Info of filename: string
    | Output of filename: string
    | OutBmp of filename: string
    | OutGif of filename: string
    | OutJpeg of filename: string
    | OutPbm of filename: string
    | OutPng of filename: string
    | OutTiff of filename: string
    | OutTga of filename: string
    | OutWebp of filename: string
    | MultSize of size: string
    | DivSize of size: string
    | Sequence of Expr list
