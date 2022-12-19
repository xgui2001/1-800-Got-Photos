module Evaluator

open Combinator
open Parser
open AST
open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing

(* Represents a variable environment *)
type Env = Map<string, Expr>

// an evaluator that evaluates the AST created by the parser and matches each expression with its corresponding type
let rec eval (e: Expr) (env: Env) : Expr * Env =
    match e with
    //Input
    | Input f ->
        let img = Image.Load(f)
        let actualimg = LoadedImage(img)
        let env2 = Map.add "x" actualimg env
        printfn "Your image %s is loaded \n" f
        actualimg, env2 //return img and the updated map
    //LoadedImage
    | LoadedImage (img) ->
        //printfn "%A" ("Image Loaded") //?
        e, env
    //Output
    | Output f ->
        if env.ContainsKey "x" then
            let value = env["x"]

            match value with
            | LoadedImage (i) ->
                i.Save(f)
                printfn "Your image %s is saved \n" f
                e, env
            | _ -> failwith ("Not a loaded image")
        else
            raise (Exception("Undefined Image '" + f + "'"))
    //OutBmp
    | OutBmp f ->
        if not (f.Contains ".bmp") then
            raise (Exception("Type mismatch"))

        if env.ContainsKey "x" then
            let value = env["x"]
            printfn "Your image %s is saved \n" f

            match value with
            | LoadedImage (i) ->
                i.SaveAsBmp(f)
                e, env
            | _ -> failwith ("Not a loaded image")
        else
            raise (Exception("Undefined Image '" + f + "'"))
    //OutGif
    | OutGif f ->
        if not (f.Contains ".gif") then
            raise (Exception("Type mismatch"))

        if env.ContainsKey "x" then
            let value = env["x"]
            printfn "Your image %s is saved \n" f

            match value with
            | LoadedImage (i) ->
                i.SaveAsGif(f)
                e, env
            | _ -> failwith ("Not a loaded image")
        else
            raise (Exception("Undefined Image '" + f + "'"))
    //OutJpeg
    | OutJpeg f ->
        if not (f.Contains ".jpeg") then
            raise (Exception("Type mismatch"))

        if env.ContainsKey "x" then
            let value = env["x"]
            printfn "Your image %s is saved \n" f

            match value with
            | LoadedImage (i) ->
                i.SaveAsJpeg(f)
                e, env
            | _ -> failwith ("Not a loaded image")
        else
            raise (Exception("Undefined Image '" + f + "'"))
    //OutPbm
    | OutPbm f ->
        if not (f.Contains ".pbm") then
            raise (Exception("Type mismatch"))

        if env.ContainsKey "x" then
            let value = env["x"]
            printfn "Your image %s is saved \n" f

            match value with
            | LoadedImage (i) ->
                i.SaveAsPbm(f)
                e, env
            | _ -> failwith ("Not a loaded image")
        else
            raise (Exception("Undefined Image '" + f + "'"))
    //OutPng
    | OutPng f ->
        if not (f.Contains ".png") then
            raise (Exception("Type mismatch"))

        if env.ContainsKey "x" then
            let value = env["x"]
            printfn "Your image %s is saved \n" f

            match value with
            | LoadedImage (i) ->
                i.SaveAsPng(f)
                e, env
            | _ -> failwith ("Not a loaded image")
        else
            raise (Exception("Undefined Image '" + f + "'"))
    //OutTiff
    | OutTiff f ->
        if not (f.Contains ".tiff") then
            raise (Exception("Type mismatch"))

        if env.ContainsKey "x" then
            let value = env["x"]
            printfn "Your image %s is saved \n" f

            match value with
            | LoadedImage (i) ->
                i.SaveAsTiff(f)
                e, env
            | _ -> failwith ("Not a loaded image")
        else
            raise (Exception("Undefined Image '" + f + "'"))
    //OutTga
    | OutTga f ->
        if not (f.Contains ".tga") then
            raise (Exception("Type mismatch"))

        if env.ContainsKey "x" then
            let value = env["x"]
            printfn "Your image %s is saved \n" f

            match value with
            | LoadedImage (i) ->
                i.SaveAsTga(f)
                e, env
            | _ -> failwith ("Not a loaded image")
        else
            raise (Exception("Undefined Image '" + f + "'"))
    //OutWebp
    | OutWebp f ->
        if not (f.Contains ".webp") then
            raise (Exception("Type mismatch"))

        if env.ContainsKey "x" then
            let value = env["x"]
            printfn "Your image %s is saved \n" f

            match value with
            | LoadedImage (i) ->
                i.SaveAsWebp(f)
                e, env
            | _ -> failwith ("Not a loaded image")
        else
            raise (Exception("Undefined Image '" + f + "'"))
    //Info
    | Info f ->
        let imgdata = Image.Identify(f)

        if f.Contains ".bmp" then
            printfn "Image Format: Bmp"

        if f.Contains ".gif" then
            printfn "Image Format: Gif"

        if f.Contains ".jpeg" then
            printfn "Image Format: Jpeg"

        if f.Contains ".pbm" then
            printfn "Image Format: Pbm"

        if f.Contains ".png" then
            printfn "Image Format: Png"

        if f.Contains ".tiff" then
            printfn "Image Format: Tiff"

        if f.Contains ".tga" then
            printfn "Image Format: Tga"

        if f.Contains ".webp" then
            printfn "Image Format: WebP"

        printfn "Image Height: %Apx" (imgdata.Height)
        printfn "Image Width: %Apx \n" (imgdata.Width)
        Info f, env
    //MultSize
    | MultSize s ->
        if env.ContainsKey "x" then
            let value = env["x"]

            match value with
            | LoadedImage (i) ->
                match Int32.TryParse(s) with
                | true, num -> i.Mutate(fun x -> x.Resize(i.Width * num, i.Height * num) |> ignore)
                | _ -> raise (Exception("Invalid integer entered"))

                printfn "Image size has been multiplied by %s \n" s
                e, env
            | _ -> failwith ("Not a loaded image")
        else
            raise (Exception("Image Undefined"))
    //DivSize
    | DivSize s ->
        if env.ContainsKey "x" then
            let value = env["x"]

            match value with
            | LoadedImage (i) ->
                match Int32.TryParse(s) with
                | true, num -> i.Mutate(fun x -> x.Resize(i.Width / num, i.Height / num) |> ignore)
                | _ -> raise (Exception("Invalid integer entered"))

                printfn "Image size has been divided by %s \n" s
                e, env
            | _ -> failwith ("Not a loaded image")
        else
            raise (Exception("Image Undefined"))
    (* Evaluates a sequence of values, returning
     * the last one in the sequence.
     *)
    | Sequence es ->
        match es with
        | [] -> failwith "Empty sequence not allowed"
        | [ e ] -> eval e env
        | e :: es' ->
            let e', env' = eval e env
            let s = Sequence es'
            eval s env'
