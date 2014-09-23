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
    member this.``1 item list``() =
        let v = "(one)"

        test Parser.parseList v |> should equal [ "one" ]

    [<TestMethod>]
    member this.``3 item list``() =
        let v = "(one two three)"

        test Parser.parseList v |> should equal [ "one"; "two"; "three" ]
    
    [<TestMethod>]
    member this.``+ symbol``() =
        let v = "(+ 1 2)"

        test Parser.parseList v |> should equal [ "+"; "1"; "2" ]
    
//    [<TestMethod>]
//    member this.``float list``() =
//        let v = "(1.0)"
//
//        test Parser.parseList v |> should equal 1.0
