namespace ComplectScheme.UnitTests

open System
open System.Reflection.Emit
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit

open ComplectScheme
open Assembly
open Expressions
open Types
open Metadata
open Compiler

module CompilerWrapper = 
    let (asmInfo : AssemblyInfo) = { AssemblyName = "complect"; EntryPointName = "Main"; MainClassName = "MainClass"; ExecutableName = "program.exe" }

    let compileExpr expr =
        let returnType = TypeInference.inferType expr
    
        let mainFunctionInfo = {
            Name = "Main";
            Body = expr;
            ReturnType = returnType;
            Parameters = [];
            Builder = None;
            IsStatic = true;
        }
        let mainTypeInfo = {
            Name  = "MainClass";
            Functions = [ mainFunctionInfo ];
            Ctors = [];
            NestedTypes = [];
            IsNested = false;
            Fields = [];
            Builder = None;
        }

        Compiler.build asmInfo mainTypeInfo

    let compileAndRunExpr expr =
        let mainType = compileExpr expr
        Compiler.drive mainType [| |]
    
open CompilerWrapper

[<TestClass>]
type Values() = 

    [<TestMethod>]
    member this.``Int immediate value``() =
        let expr = Expr.Immediate(Value.Int(5))
        let ret = compileAndRunExpr expr

        ret |> should equal 5

    [<TestMethod>]
    member this.``Char immediate value``() =
        let expr = Expr.Immediate(Value.Char('a'))
        let ret = compileAndRunExpr expr

        ret |> should equal 'a'

    [<TestMethod>]
    member this.``Bool immediate value``() =
        let expr = Expr.Immediate(Value.Bool(true))
        let ret = compileAndRunExpr expr

        ret |> should equal true

    [<TestMethod>]
    member this.``Null immediate value``() =
        let expr = Expr.Immediate(Value.Null)
        let ret = compileAndRunExpr expr

        ret |> should equal null

    [<TestMethod>]
    member this.``List of ints``() =
        let expr = Expr.Immediate(Value.List([Value.Int(1); Value.Int(2)]))
        let ret = compileAndRunExpr expr

        ret |> should equal (Tuple.Create(1, Tuple.Create(2, null)))

    [<TestMethod>]
    member this.``Pair of ints``() =
        let expr = Expr.Immediate(Value.Pair(Value.Int(1), Value.Int(2)))
        let ret = compileAndRunExpr expr

        ret |> should equal (System.Tuple<int, int>(1, 2))

[<TestClass>]
type UnaryOperations() =

    [<TestMethod>]
    member this.``Add1``() =
        let expr = Expr.UnaryOperation(UnaryOp.Add1, Expr.Immediate(Value.Int(5)))
        let ret = compileAndRunExpr expr

        ret |> should equal 6

    [<TestMethod>]
    member this.``Sub1``() =
        let expr = Expr.UnaryOperation(UnaryOp.Sub1, Expr.Immediate(Value.Int(5)))
        let ret = compileAndRunExpr expr

        ret |> should equal 4

    [<TestMethod>]
    member this.``IsZero on 0 is true``() =
        let expr = Expr.UnaryOperation(UnaryOp.IsZero, Expr.Immediate(Value.Int(0)))
        let ret = compileAndRunExpr expr

        ret |> should equal true

    [<TestMethod>]
    member this.``IsNull on null is true``() =
        let expr = Expr.UnaryOperation(UnaryOp.IsNull, Expr.Immediate(Value.Null))
        let ret = compileAndRunExpr expr

        ret |> should equal true

[<TestClass>]
type BinaryOperations() =
    
    [<TestMethod>]
    member this.``Add with positive integers``() =
        let expr = Expr.BinaryOperation(BinaryOp.Add, Expr.Immediate(Value.Int(3)), Expr.Immediate(Value.Int(2)))
        let ret = compileAndRunExpr expr

        ret |> should equal 5

    [<TestMethod>]
    member this.``Sub with positive integers``() =
        let expr = Expr.BinaryOperation(BinaryOp.Sub, Expr.Immediate(Value.Int(5)), Expr.Immediate(Value.Int(2)))
        let ret = compileAndRunExpr expr

        ret |> should equal 3

[<TestClass>]
type CompositeExpressions() =
    
    [<TestMethod>]
    member this.``Sub followed by IsZero``() =
        let expr = 
            Expr.UnaryOperation(
                UnaryOp.IsZero, 
                Expr.BinaryOperation(BinaryOp.Sub, Expr.Immediate(Value.Int(5)), Expr.Immediate(Value.Int(5))))
        let ret = compileAndRunExpr expr

        ret |> should equal true

[<TestClass>]
type LetBindings() =

    let foo = ("foo", Expr.Immediate(Value.Int(5)))
    let bar = ("bar", Expr.Immediate(Value.Int(10)))

    [<TestMethod>]
    member this.``Let binding and variable ref``() =
        let expr =
            Expr.LetBinding(
                [foo],
                Expr.BinaryOperation(
                    BinaryOp.Add,
                    Expr.Immediate(Value.Int(10)),
                    Expr.VariableRef("foo", typeof<int>)))
        let ret = compileAndRunExpr expr

        ret |> should equal 15
    
    [<TestMethod>]
    member this.``Multiple bindings in one let``() =
        let expr = 
            Expr.LetBinding(
                [foo;
                 bar],
                Expr.BinaryOperation(
                    BinaryOp.Add,
                    Expr.VariableRef("foo", typeof<int>),
                    Expr.VariableRef("bar", typeof<int>)))
        let ret = compileAndRunExpr expr

        ret |> should equal 15

    [<TestMethod>]
    member this.``Nested let bindings``() =
        let expr = 
            Expr.LetBinding(
                [foo],
                Expr.LetBinding(
                    [bar],
                    Expr.BinaryOperation(
                        BinaryOp.Add,
                        Expr.VariableRef("foo", typeof<int>),
                        Expr.VariableRef("bar", typeof<int>))))
        let ret = compileAndRunExpr expr

        ret |> should equal 15

    [<TestMethod>]
    member this.``Variable shadowing``() =
        let expr = 
            Expr.LetBinding(
                [foo],
                Expr.LetBinding(
                    [foo],
                    Expr.BinaryOperation(
                        BinaryOp.Add,
                        Expr.VariableRef("foo", typeof<int>),
                        Expr.Immediate(Value.Int(1)))))
        let ret = compileAndRunExpr expr

        ret |> should equal 6

    [<TestMethod>]
    member this.``Expression in let binding``() =
        let expr = 
            Expr.LetBinding(
                [("foo", Expr.UnaryOperation(UnaryOp.Add1, Expr.Immediate(Value.Int(5))))],
                Expr.BinaryOperation(
                    BinaryOp.Add,
                    Expr.VariableRef("foo", typeof<int>),
                    Expr.Immediate(Value.Int(10))))
        let ret = compileAndRunExpr expr

        ret |> should equal 16

    [<TestMethod>]
    member this.``Reference to unknown variable``() =
        let expr = 
            Expr.LetBinding(
                [("foo", Expr.UnaryOperation(UnaryOp.Add1, Expr.Immediate(Value.Int(5))))],
                Expr.BinaryOperation(
                    BinaryOp.Add,
                    Expr.VariableRef("bar", typeof<int>),
                    Expr.Immediate(Value.Int(10))))
        
        (fun () -> compileAndRunExpr expr |> ignore) |> should throw typeof<System.Exception>

[<TestClass>]
type Conditionals() =

    [<TestMethod>]
    member this.``If then else with true condition``() =
        let expr =
            Expr.Conditional(
                Expr.UnaryOperation(UnaryOp.IsZero, Expr.Immediate(Value.Int(0))),
                Expr.Immediate(Value.Bool(true)),
                Expr.Immediate(Value.Bool(false)))
        let ret = compileAndRunExpr expr

        ret |> should equal true

    [<TestMethod>]
    member this.``If then else with false condition``() =
        let expr =
            Expr.Conditional(
                Expr.UnaryOperation(UnaryOp.IsZero, Expr.Immediate(Value.Int(1))),
                Expr.Immediate(Value.Bool(true)),
                Expr.Immediate(Value.Bool(false)))
        let ret = compileAndRunExpr expr

        ret |> should equal false

[<TestClass>]
type Lambdas() =

    let foo = ("foo", Expr.Immediate(Value.Int(5)))
    let bar = ("bar", Expr.Immediate(Value.Int(2)))

    [<TestMethod>]
    member this.``Basic lambda, no captures``() =
        let expr =
            Expr.FunctionCall(
                Expr.Lambda(
                    [("bar", typeof<int>)],
                    [ ],
                    Expr.BinaryOperation(
                        BinaryOp.Add,
                        Expr.VariableRef("bar", typeof<int>),
                        Expr.Immediate(Value.Int(1)))),
                [bar])
        let ret = compileAndRunExpr expr

        ret |> should equal 3

    [<TestMethod>]
    member this.``Basic lambda with captures, no formal parameters``() =
        let expr =
            Expr.FunctionCall(
                Expr.LetBinding(
                    [foo; bar],
                    Expr.Lambda(
                        [],
                        [("bar", typeof<int>); ("foo", typeof<int>)],
                        Expr.BinaryOperation(
                            BinaryOp.Add,
                            Expr.VariableRef("bar", typeof<int>),
                            Expr.VariableRef("foo", typeof<int>)))),
                [])
        let ret = compileAndRunExpr expr

        ret |> should equal 7

    [<TestMethod>]
    member this.``Basic lambda with captured variable and formal parameter``() =
        let expr =
            Expr.FunctionCall(
                Expr.LetBinding(
                    [foo],
                    Expr.Lambda(
                        [("bar", typeof<int>)],
                        [("foo", typeof<int>)],
                        Expr.BinaryOperation(
                            BinaryOp.Add,
                            Expr.VariableRef("bar", typeof<int>),
                            Expr.VariableRef("foo", typeof<int>)))),
                [bar])
        let ret = compileAndRunExpr expr

        ret |> should equal 7
