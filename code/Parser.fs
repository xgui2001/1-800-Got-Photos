module Parser

open Combinator

open AST

// a list of words that should never be variable names
let reserved = [ "Input"; "Output" ]

//declare expression parser so we can use it recursively *)
let pexpr, pexprImpl = recparser ()

// a parser to parse the filename of the image
let filename =
    pmany1 (pletter <|> pchar '.' <|> pdigit) |>> (stringify) <!> "filename"

// a parser to handle the quotaion marks
let literal = pbetween (pchar '"') (pchar '"') filename <!> "literal"

// a parser to parse in Input
let pinput =
    pright (pleft (pstr "Input") (pws1)) literal |>> (fun a -> Input a) <!> "pinput"

// a parser to parse in Output
let poutput =
    pright (pleft (pstr "Output") (pws1)) literal |>> (fun a -> Output a)
    <!> "poutput"

// // a parser to parse in OutputBMG
// let pbmg =
//     pright (pleft (pstr "OutputBMG") (pws1)) literal |>> (fun a -> Output a)
//     <!> "pbmg"

// // a parser to parse in OutputGIF
// let pgif =
//     pright (pleft (pstr "OutputGIF") (pws1)) literal |>> (fun a -> Output a)
//     <!> "pgif"

// // a parser to parse in OutputJPEG
// let pjpeg =
//     pright (pleft (pstr "OutputGIF") (pws1)) literal |>> (fun a -> Output a)
//     <!> "pjpeg"

// a parser to parse in Info
let pinfo =
    pright (pleft (pstr "Info") (pws1)) literal |>> (fun a -> Info a) <!> "pinfo"


// defines whitespace
let whitespace = (pwsNoNL0 |>> (fun _ -> true))

(* pvar
 *   Parses a variable.  Variable names are at least one
 *   character long, starting with a letter, followed by
 *   any combination of letters or numbers.
 *)
let pvarchar: Parser<char> = pletter <|> pdigit <!> "pvarchar"

let pvar: Parser<Expr> =
    pseq pletter (pmany0 pvarchar |>> stringify) (fun (c: char, s: string) -> (string c) + s)
    |>> (fun v ->
        if List.contains v reserved then
            failwith ("'" + v + "' is a reserved word.")
        else
            Variable v)
    <!> "pvar"

(* pad p
 *   Parses p, surrounded by optional whitespace.
 *)
let pad p = pbetween whitespace whitespace p

(* passign
 *   Parses an assignment, e.g.,
 *   x = 2
 *)
let passign =
    pseq (pleft (pad pvar) (pad (pstr "="))) (pad pexpr) Assignment <!> "passign"

(* pexprs
 *  Parses a sequence of expressions.  Sequences are
 *  delimited by whitespace (usually newlines).
 *)
let pexprs = pmany1 (pleft (pad pexpr) pws0) |>> Sequence <!> "pexprs"

// a parser that converts the input to an expr (?)
pexprImpl := pinput <|> poutput <|> pinfo <|> pvar <|> passign <!> "pexpr"


// a grammar parser that takes in an expr and checks whether it has reached the end of the input
let grammar = pleft pexprs peof <!> "grammar"
