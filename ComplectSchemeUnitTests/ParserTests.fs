namespace ComplectScheme.UnitTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit

open FParsec
open Parser

[<TestClass>]
type ParserTests() =
    
    let test parser input =
        let r = run parser input
        match r with 
            | ParserResult.Success(x, _, _) -> x
            | ParserResult.Failure(x, _, _) -> failwith x

    [<TestMethod>]
    member this.``1 symbol list``() =
        let v = "(one)"
        test Parser.parse v |> should equal (Identifier.List([ Identifier.Symbol("one") ]))

    [<TestMethod>]
    member this.``3 symbol list``() =
        let v = "(one two three)"
        test Parser.parse v |> should equal (Identifier.List([ Identifier.Symbol("one"); Identifier.Symbol("two"); Identifier.Symbol("three") ]))
    
    [<TestMethod>]
    member this.``+ symbol``() =
        let v = "(+)"
        test Parser.parse v |> should equal (Identifier.List([ Identifier.Symbol("+") ]))

    [<TestMethod>]
    member this.``String literal``() =
        let v = "(\"foo_bar123'\")"
        test Parser.parse v |> should equal (Identifier.List([ Identifier.String("foo_bar123'") ]))
