module Parser

open Combinator

open AST

// a list of words that should never be variable names
let reserved = []

//declare expression parser so we can use it recursively *)
let pexpr, pexprImpl = recparser ()

// a parser to parse the filename of the image
let filename = pmany1 (pletter <|> pchar '.' <|> pdigit) |>> (stringify) //<!> "filename"

// a parser to parse size
let size = pmany1 (pdigit) |>> (stringify) // <!> "size"

// a parser to parse pmultsize
let pmultsize =
    pright (pleft (pstr "MultSize") (pws1)) size |>> (fun a -> MultSize a)
//<!> "pmultsize"

// a parser to parse pdivsize
let pdivsize = pright (pleft (pstr "DivSize") (pws1)) size |>> (fun a -> DivSize a)
//<!> "pdivsize"

// a parser to handle the quotaion marks
let literal = pbetween (pchar '"') (pchar '"') filename //<!> "literal"

// a parser to parse in In
let pinput = pright (pleft (pstr "Input") (pws1)) literal |>> (fun a -> Input a) //<!> "pin"

// a parser to parse in Out
let poutput = pright (pleft (pstr "Output") (pws1)) literal |>> (fun a -> Output a) //<!> "pout"

// a parser to parse in OutBmp
let pbmp = pright (pleft (pstr "OutBmp") (pws1)) literal |>> (fun a -> OutBmp a) //<!> "pbmp"

// a parser to parse in OutGif
let pgif = pright (pleft (pstr "OutGif") (pws1)) literal |>> (fun a -> OutGif a) //<!> "pgif"

// a parser to parse in OutJpeg
let pjpeg = pright (pleft (pstr "OutJpeg") (pws1)) literal |>> (fun a -> OutJpeg a)
//<!> "pjpeg"

// a parser to parse in OutPbm
let ppbm = pright (pleft (pstr "OutPbm") (pws1)) literal |>> (fun a -> OutPbm a) //<!> "ppbm"

// a parser to parse in OutPng
let ppng = pright (pleft (pstr "OutPng") (pws1)) literal |>> (fun a -> OutPng a) //<!> "ppng"

// a parser to parse in OutTiff
let ptiff = pright (pleft (pstr "OutTiff") (pws1)) literal |>> (fun a -> OutTiff a)
//<!> "ptiff"

// a parser to parse in OutTga
let ptga = pright (pleft (pstr "OutTga") (pws1)) literal |>> (fun a -> OutTga a) //<!> "ptga"

// a parser to parse in OutWebp
let pwebp = pright (pleft (pstr "OutWebp") (pws1)) literal |>> (fun a -> OutWebp a)
//<!> "pwebp"

// a parser to parse in Info
let pinfo = pright (pleft (pstr "Info") (pws1)) literal |>> (fun a -> Info a) //<!> "pinfo"

// defines whitespace
let whitespace = (pwsNoNL0 |>> (fun _ -> true))

(* pad p
 *   Parses p, surrounded by optional whitespace.
 *)
let pad p = pbetween whitespace whitespace p

(* pexprs
 *  Parses a sequence of expressions.  Sequences are
 *  delimited by whitespace (usually newlines).
 *)
let pexprs = pmany1 (pleft (pad pexpr) pws0) |>> Sequence //<!> "pexprs"

// a parser that converts the input to an expr (?)
pexprImpl
:= pinput
   <|> poutput
   <|> pinfo
   <|> pmultsize
   <|> pdivsize
   <|> pbmp
   <|> pgif
   <|> pjpeg
   <|> ppbm
   <|> ppng
   <|> ptiff
   <|> ptga
   <|> pwebp
//<!> "pexpr"


// a grammar parser that takes in an expr and checks whether it has reached the end of the input
let grammar = pleft pexprs peof //<!> "grammar"
