namespace actualtest

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open Evaluator
open AST

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

    //Test whether parse correctly
    [<TestMethod>]
    member this.TestP() =
        let program = "Info 'Cat.jpeg'"
        let expected = Info "Cat.jpeg"
        let ast_maybe = parse program

        match ast_maybe with
        | Some ast ->
            let actual, _ = eval ast Map.empty
            Assert.AreEqual(expected, actual)
        | None -> Assert.IsTrue(false)
