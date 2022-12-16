module Evaluator

open Combinator
open Parser
open AST
open System
open SixLabors.ImageSharp

(* Represents a variable environment *)
type Env = Map<string, Expr>

// an evaluator that evaluates the AST created by the parser and matches each expression with its corresponding type
let rec eval (e: Expr) (env: Env) : Expr * Env =
    match e with
    | Input f ->
        let img = Image.Load(f)
        let actualimg = LoadedImage(img) //varaible
        let env2 = Map.add "x" actualimg env
        actualimg, env2 //returning actualinmg anmd uodate map
    | LoadedImage (img) ->
        printfn "%A" ("Loaded Image") //?
        e, env
    | Output f ->
        if env.ContainsKey "x" then
            let value = env["x"]

            match value with
            | LoadedImage (i) ->
                i.Save(f)
                e, env
            | _ -> failwith ("x is not a loaded image")
        else
            //failwith ("Undefined Image '" + f + "'")
            raise (Exception("Undefined Image '" + f + "'"))
    | Info f ->
        let imgdata = Image.Identify(f)
        printfn "Height: %Apx" (imgdata.Height)
        printfn "Width: %Apx" (imgdata.Width)
        Info f, env
    (* Evaluating a variable returns its value in
     * the environment. Fails if the variable is not
     * in the environment.
     *)
    | Variable v ->
        if env.ContainsKey v then
            let value = env.Item v
            value, env
        else
            failwith ("Undefined variable '" + v + "'")
    (* Assignment evaluates the right hand side, then
     * stores that evaluated result in the variable
     * indicated by the left hand side.  Fails if the
     * left hand side is not a variable.
     *)
    | Assignment (e1, e2) ->
        match e1 with
        | Variable v ->
            let e2', env' = eval e2 env
            let env'' = env'.Add(v, e2')
            e2', env''
        | _ -> failwith "Left side of assignment must be a variable."
    (* Evaluates a sequence of values, returning
     * the last one in the sequence.
     *)
    | Sequence es ->
        match es with
        | [] -> failwith "Empty sequence not allowed."
        | [ e ] -> eval e env
        | e :: es' ->
            let e', env' = eval e env
            let s = Sequence es'
            eval s env'
    | No -> failwith ("shouldn't happen")






(*// an evaluator that evaluates the AST created by the parser and matches each expression with its corresponding type
let rec evaluate (e: Expr) : Expr =
    match e with
    | Input f ->
        let img = Image.Load(f)
        LoadedImage(img)
    | Output f ->
        let img = Image.Save(f)
        LoadedImage(img)
    | Info f ->
        let imgdata = Image.Identify(f)
        DataImage(imgdata)
    | LoadedImage (img) ->
        printfn "%A" (img.Save("Loaded Image")) //?
        e
    | DataImage (imgdata) ->
        printfn "%A" (imgdata.Height)
        printfn "%A" (imgdata.Width)
        printfn "%A" (imgdata.PixelType)
        printfn "%A" (imgdata.Metadata)
        e
    | No -> failwith ("shouldn't happen")

*)
