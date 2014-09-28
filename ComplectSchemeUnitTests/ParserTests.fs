namespace ComplectScheme.UnitTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit

open FParsec
open Parser
open Expressions

module ParserTestUtils =
    let test parser input =
        let r = run parser input
        match r with 
            | ParserResult.Success(x, _, _) -> x
            | ParserResult.Failure(x, _, _) -> failwith x

    let testAll parser l =
        l |> List.map (fun v -> test parser v)

open ParserTestUtils

[<TestClass>]
type DatumTests() =

    [<TestMethod>]
    member this.``Empty list``() =
        let v = "()"
        test Parser.parseDatum v |> should equal (Datum.List([]))

    [<TestMethod>]
    member this.``1 symbol list``() =
        let v = "(one)"
        test Parser.parseDatum v |> should equal (Datum.List([ Datum.Symbol("one") ]))

    [<TestMethod>]
    member this.``3 symbol list``() =
        let v = "(one two three)"
        test Parser.parseDatum v |> should equal (Datum.List([ Datum.Symbol("one"); Datum.Symbol("two"); Datum.Symbol("three") ]))
    
    [<TestMethod>]
    member this.``List with []``() =
        let v = "[one two]"
        test Parser.parseDatum v |> should equal (Datum.List([ Datum.Symbol("one"); Datum.Symbol("two") ]))
    
    [<TestMethod>]
    member this.``List with {}``() =
        let v = "{one two}"
        test Parser.parseDatum v |> should equal (Datum.List([ Datum.Symbol("one"); Datum.Symbol("two") ]))

    [<TestMethod>]
    member this.``Symbols``() =
        let v = ["+"; "-"; "foo"]
        testAll Parser.parseDatum v |> should equal [ Datum.Symbol("+"); Datum.Symbol("-"); Datum.Symbol("foo") ]

    [<TestMethod>]
    member this.``String literals``() =
        let v = ["\"foo bar\""; "\"1'2_3\""]
        testAll Parser.parseDatum v |> should equal [ Datum.String("foo bar"); Datum.String("1'2_3") ]

    [<TestMethod>]
    member this.``Integers``() =
        let v = ["1"; "-20"]
        testAll Parser.parseDatum v |> should equal [ Datum.Number(1); Datum.Number(-20) ]

    [<TestMethod>]
    member this.``Valid Booleans``() =
        let v = ["#t"; "#f"]
        testAll Parser.parseDatum v |> should equal [ Datum.Boolean(true); Datum.Boolean(false) ]

    [<TestMethod>]
    member this.``Characters``() =
        let v = ["#\\A"; "#\\f"]
        testAll Parser.parseDatum v |> should equal [ Datum.Character('A'); Datum.Character('f') ]

    [<TestMethod>]
    member this.``Heterogeneous list``() =
        let v = "(+ \"bar\" 1 #t #\\A (a . b) (foo))"
        test Parser.parseDatum v |> should equal 
            (Datum.List(
                [ Datum.Symbol("+"); 
                  Datum.String("bar"); 
                  Datum.Number(1); 
                  Datum.Boolean(true);
                  Datum.Character('A');
                  Datum.Pair(Datum.Symbol("a"), Datum.Symbol("b"));
                  Datum.List([ Datum.Symbol("foo") ]) ]))

    [<TestMethod>]
    member this.``Nested lists``() =
        let v = "(+ (foo 1 (\"bar\")) baz)"
        test Parser.parseDatum v |> should equal
            (Datum.List(
                [ Datum.Symbol("+");
                  Datum.List(
                    [ Datum.Symbol("foo");
                      Datum.Number(1);
                      Datum.List([ Datum.String("bar") ]) ]);
                  Datum.Symbol("baz") ]))

    [<TestMethod>]
    member this.``Pair in ()``() =
        let v = "(foo . bar)"
        test Parser.parseDatum v |> should equal (Datum.Pair(Datum.Symbol("foo"), Datum.Symbol("bar")))

    [<TestMethod>]
    member this.``Pair in []``() =
        let v = "[foo . bar]"
        test Parser.parseDatum v |> should equal (Datum.Pair(Datum.Symbol("foo"), Datum.Symbol("bar")))

    [<TestMethod>]
    member this.``Pair in {}``() =
        let v = "{foo . bar}"
        test Parser.parseDatum v |> should equal (Datum.Pair(Datum.Symbol("foo"), Datum.Symbol("bar")))

    [<TestMethod>]
    member this.``Pairs, no space``() =
        let v = "(foo.bar)"
        test Parser.parseDatum v |> should equal (Datum.Pair(Datum.Symbol("foo"), Datum.Symbol("bar")))

    [<TestMethod>]
    member this.``Pairs, with int``() =
        let v = "(1.bar)"
        test Parser.parseDatum v |> should equal (Datum.Pair(Datum.Number(1), Datum.Symbol("bar")))

[<TestClass>]
type ExpressionTests() =

    [<TestMethod>]
    member this.``Quoted list``() =
        let v = "'(foo bar)"
        test Parser.parseExpr v |> should equal (Expression.Quote(Datum.List([ Datum.Symbol("foo"); Datum.Symbol("bar") ])))

    [<TestMethod>]
    member this.``Quoted value``() =
        let v = "'foo"
        test Parser.parseExpr v |> should equal (Expression.Quote(Datum.Symbol("foo")))

//    [<TestMethod>]
//    member this.``Line comment``() =
//        let v = "#t ; this is a comment\n#f"
//        test Parser.parseAll v |> should equal [ Datum.Boolean(true); Datum.Comment(" this is a comment"); Datum.Boolean(false) ]

//[<TestClass>]
//type ExpressionParsing() =
//
//    [<TestMethod>]
//    member this.``Quoted list``() =
//        let v = "(quote (#t 1))"
//        parseString v |> should equal (Expr.Immediate(Value.List([ Value.Boolean(true); Value.Number(1) ])))
//
//    [<TestMethod>]
//    member this.``Parse bool``() =
//        let v = "#t"
//        parseString v |> should equal (Expr.Immediate(Value.Boolean(true)))
//
//    [<TestMethod>]
//    member this.``Parse int``() =
//        let v = "3"
//        parseString v |> should equal (Expr.Immediate(Value.Number(3)))
