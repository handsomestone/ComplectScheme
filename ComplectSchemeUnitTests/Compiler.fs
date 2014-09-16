namespace ComplectScheme.UnitTests

open System.Reflection.Emit
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit

open ComplectScheme
open Assembly
open Expressions
open Metadata
open Compiler

module CompilerWrapper = 
    let (asmInfo : AssemblyInfo) = { AssemblyName = "complect"; EntryPointName = "Main"; MainClassName = "MainClass"; ExecutableName = "program.exe" }

    let compileExpr expr =
        let mainFunctionInfo = {
            Name = "Main";
            Body = expr;
            ReturnType = Some(typeof<int>)
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
type PrimitiveTypes() = 

    [<TestMethod>]
    member this.``Int immediate value``() =
        let expr = Expr.Immediate(Value.Int(5))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeValue (Value.Int(5)))

    [<TestMethod>]
    member this.``Char immediate value``() =
        let expr = Expr.Immediate(Value.Char('a'))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeValue (Value.Char('a')))

    [<TestMethod>]
    member this.``Bool immediate value``() =
        let expr = Expr.Immediate(Value.Bool(true))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeValue (Value.Bool(true)))

    [<TestMethod>]
    member this.``Null immediate value``() =
        let expr = Expr.Immediate(Value.Null)
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeValue (Value.Null))

[<TestClass>]
type UnaryOperations() =

    [<TestMethod>]
    member this.``Add1``() =
        let expr = Expr.UnaryOperation(UnaryOp.Add1, Expr.Immediate(Value.Int(5)))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeInt 6)

    [<TestMethod>]
    member this.``Sub1``() =
        let expr = Expr.UnaryOperation(UnaryOp.Sub1, Expr.Immediate(Value.Int(5)))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeInt 4)

    [<TestMethod>]
    member this.``IsZero on 0 is true``() =
        let expr = Expr.UnaryOperation(UnaryOp.IsZero, Expr.Immediate(Value.Int(0)))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeBool true)

    [<TestMethod>]
    member this.``IsNull on null is true``() =
        let expr = Expr.UnaryOperation(UnaryOp.IsNull, Expr.Immediate(Value.Null))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeBool true)

[<TestClass>]
type BinaryOperations() =
    
    [<TestMethod>]
    member this.``Add with positive integers``() =
        let expr = Expr.BinaryOperation(BinaryOp.Add, Expr.Immediate(Value.Int(3)), Expr.Immediate(Value.Int(2)))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeInt 5)

    [<TestMethod>]
    member this.``Sub with positive integers``() =
        let expr = Expr.BinaryOperation(BinaryOp.Sub, Expr.Immediate(Value.Int(5)), Expr.Immediate(Value.Int(2)))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeInt 3)

[<TestClass>]
type CompositeExpressions() =
    
    [<TestMethod>]
    member this.``Sub followed by IsZero``() =
        let expr = 
            Expr.UnaryOperation(
                UnaryOp.IsZero, 
                Expr.BinaryOperation(BinaryOp.Sub, Expr.Immediate(Value.Int(5)), Expr.Immediate(Value.Int(5))))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeBool true)

[<TestClass>]
type LetBindings() =

    [<TestMethod>]
    member this.``Let binding and variable ref``() =
        let expr =
            Expr.LetBinding(
                [("foo", Expr.Immediate(Value.Int(5)))],
                Expr.BinaryOperation(
                    BinaryOp.Add,
                    Expr.Immediate(Value.Int(10)),
                    Expr.VariableRef("foo")))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeInt 15)
    
    [<TestMethod>]
    member this.``Multiple bindings in one let``() =
        let expr = 
            Expr.LetBinding(
                [("foo", Expr.Immediate(Value.Int(5)));
                 ("bar", Expr.Immediate(Value.Int(10)))],
                Expr.BinaryOperation(
                    BinaryOp.Add,
                    Expr.VariableRef("foo"),
                    Expr.VariableRef("bar")))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeInt 15)

    [<TestMethod>]
    member this.``Nested let bindings``() =
        let expr = 
            Expr.LetBinding(
                [("foo", Expr.Immediate(Value.Int(5)))],
                Expr.LetBinding(
                    [("bar", Expr.Immediate(Value.Int(10)))],
                    Expr.BinaryOperation(
                        BinaryOp.Add,
                        Expr.VariableRef("foo"),
                        Expr.VariableRef("bar"))))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeInt 15)

    [<TestMethod>]
    member this.``Variable shadowing``() =
        let expr = 
            Expr.LetBinding(
                [("foo", Expr.Immediate(Value.Int(5)))],
                Expr.LetBinding(
                    [("foo", Expr.Immediate(Value.Int(10)))],
                    Expr.BinaryOperation(
                        BinaryOp.Add,
                        Expr.VariableRef("foo"),
                        Expr.Immediate(Value.Int(1)))))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeInt 11)

    [<TestMethod>]
    member this.``Expression in let binding``() =
        let expr = 
            Expr.LetBinding(
                [("foo", Expr.UnaryOperation(UnaryOp.Add1, Expr.Immediate(Value.Int(5))))],
                Expr.BinaryOperation(
                    BinaryOp.Add,
                    Expr.VariableRef("foo"),
                    Expr.Immediate(Value.Int(10))))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeInt 16)

    [<TestMethod>]
    member this.``Reference to unknown variable``() =
        let expr = 
            Expr.LetBinding(
                [("foo", Expr.UnaryOperation(UnaryOp.Add1, Expr.Immediate(Value.Int(5))))],
                Expr.BinaryOperation(
                    BinaryOp.Add,
                    Expr.VariableRef("bar"),
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

        ret |> should equal (PrimitiveTypes.encodeBool(true))

    [<TestMethod>]
    member this.``If then else with false condition``() =
        let expr =
            Expr.Conditional(
                Expr.UnaryOperation(UnaryOp.IsZero, Expr.Immediate(Value.Int(1))),
                Expr.Immediate(Value.Bool(true)),
                Expr.Immediate(Value.Bool(false)))
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeBool(false))

[<TestClass>]
type Lambdas() =

    [<TestMethod>]
    member this.``Basic lambda``() =
        let expr =
            Expr.FunctionCall(
                Expr.LetBinding(
                    [("foo", Expr.Immediate(Value.Int(1)))],
                    Expr.Lambda(
                        ["bar"],
                        ["foo"],
                        Expr.BinaryOperation(
                            BinaryOp.Add,
                            Expr.VariableRef("bar"),
                            Expr.VariableRef("foo")))),
                [("bar", Expr.Immediate(Value.Int(2)))])
        let ret = compileAndRunExpr expr

        ret |> should equal (PrimitiveTypes.encodeInt(3))
