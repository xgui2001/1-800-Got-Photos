module Parser

open Combinator

open AST

// a parser to parse the filename of the image
let filename = pmany1 (pletter <|> pchar '.') |>> (stringify) <!> "filename"

// a parser to handle the quotaion marks of a filename
let literal = pbetween (pchar '"') (pchar '"') filename <!> "literal"

// a parser to parse in the Input command
let pinput =
    pright (pleft (pstr "Input") (pws1)) literal |>> (fun a -> Input a) <!> "pinput"

let expr = pinput <!> "expr"

// a grammar parser that takes in an expr and checks whether it has reached the end of the input
let grammar = pleft expr peof <!> "grammar"
