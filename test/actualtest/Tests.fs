namespace actualtest

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SixLabors.ImageSharp
open Parser
open Combinator
open Evaluator
open AST

type Env = Map<string, Expr>

[<TestClass>]
type TestClass() =

    //Test whether evaluator evaluates info correctly
    //Image Cat.jpeg is in the bin DO NOT DELETE
    [<TestMethod>]
    member this.TestInfo() =
        let ast = Info "Cat.jpeg"
        let expected = Info "Cat.jpeg"
        let actual, _ = eval ast Map.empty
        Assert.AreEqual(expected, actual)

    //Test whether parser parses MultSize correctly
    [<TestMethod>]
    member this.TestP() =
        let img = Image.Load("Cat.jpeg")
        let actualimg = LoadedImage(img)
        let env = Map.empty
        let env2 = Map.add "x" actualimg env
        let program = prepare "MultSize 2"
        let expected = MultSize "2"
        let ast_maybe = pmultsize program

        match ast_maybe with
        | Success (ast, _) ->
            let actual, _ = eval ast env2
            Assert.AreEqual(expected, actual)
        | Failure (_, _) -> Assert.IsTrue(false)
