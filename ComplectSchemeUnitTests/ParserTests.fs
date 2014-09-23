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

    let testAll parser l =
        l |> List.map (fun v -> test parser v)

    [<TestMethod>]
    member this.``1 symbol list``() =
        let v = "(one)"
        test Parser.parse v |> should equal (Identifier.List([ Identifier.Symbol("one") ]))

    [<TestMethod>]
    member this.``3 symbol list``() =
        let v = "(one two three)"
        test Parser.parse v |> should equal (Identifier.List([ Identifier.Symbol("one"); Identifier.Symbol("two"); Identifier.Symbol("three") ]))
    
    [<TestMethod>]
    member this.``Symbols``() =
        let v = ["+"; "-"; "foo"]
        testAll Parser.symbol v |> should equal [ Identifier.Symbol("+"); Identifier.Symbol("-"); Identifier.Symbol("foo") ]

    [<TestMethod>]
    member this.``String literals``() =
        let v = ["\"foo bar\""; "\"1'2_3\""]
        testAll Parser.stringLiteral v |> should equal [ Identifier.String("foo bar"); Identifier.String("1'2_3") ]

    [<TestMethod>]
    member this.``Integers``() =
        let v = ["1"; "-20"]
        testAll Parser.integer v |> should equal [ Identifier.Int(1); Identifier.Int(-20) ]

    [<TestMethod>]
    member this.``Booleans``() =
        let v = ["#t"; "#f"]
        testAll Parser.boolean v |> should equal [ Identifier.Bool(true); Identifier.Bool(false) ]

    [<TestMethod>]
    member this.``Heterogeneous list``() =
        let v = "(+ \"bar\" 1 #t)"
        test Parser.parse v |> should equal 
            (Identifier.List(
                [ Identifier.Symbol("+"); 
                  Identifier.String("bar"); 
                  Identifier.Int(1); 
                  Identifier.Bool(true) ]))

    [<TestMethod>]
    member this.``Nested lists``() =
        let v = "(+ (foo 1 (\"bar\")) baz)"
        test Parser.parse v |> should equal
            (Identifier.List(
                [ Identifier.Symbol("+");
                  Identifier.List(
                    [ Identifier.Symbol("foo");
                      Identifier.Int(1);
                      Identifier.List([ Identifier.String("bar") ]) ]);
                  Identifier.Symbol("baz") ]))
