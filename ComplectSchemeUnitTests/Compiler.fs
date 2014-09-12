namespace ComplectScheme.UnitTests

open System.Reflection.Emit
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit
open ComplectScheme

open Compiler

module CompilerWrapper = 
    let (asmInfo : AssemblyInfo) = { AssemblyName = "complect"; EntryPointName = "Main"; MainClassName = "MainClass"; ExecutableName = "program.exe" }

    let generateMain expr (ilGen : ILGenerator) =
        let emitter = new ILEmitter(ilGen)
        let env = new Env(None, None)
        emitter.EmitExpr expr env
        ilGen.Emit(OpCodes.Ret)
    
    let compile expr =
        Compiler.compile asmInfo asmInfo.ExecutableName (generateMain expr)

    let compileAndRun expr =
        let mainType = compile expr
        Compiler.drive mainType [| Array.empty<string> |]
    
open CompilerWrapper

[<TestClass>]
type PrimitiveTypes() = 

    [<TestMethod>]
    member this.``Int immediate value``() =
        let expr = Expr.Immediate(Value.Int(5))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeValue (Value.Int(5)))

    [<TestMethod>]
    member this.``Char immediate value``() =
        let expr = Expr.Immediate(Value.Char('a'))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeValue (Value.Char('a')))

    [<TestMethod>]
    member this.``Bool immediate value``() =
        let expr = Expr.Immediate(Value.Bool(true))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeValue (Value.Bool(true)))

    [<TestMethod>]
    member this.``Null immediate value``() =
        let expr = Expr.Immediate(Value.Null)
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeValue (Value.Null))

[<TestClass>]
type UnaryOperations() =

    [<TestMethod>]
    member this.``Add1``() =
        let expr = Expr.UnaryOperation(UnaryOp.Add1, Expr.Immediate(Value.Int(5)))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeInt 6)

    [<TestMethod>]
    member this.``Sub1``() =
        let expr = Expr.UnaryOperation(UnaryOp.Sub1, Expr.Immediate(Value.Int(5)))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeInt 4)

    [<TestMethod>]
    member this.``IsZero on 0 is true``() =
        let expr = Expr.UnaryOperation(UnaryOp.IsZero, Expr.Immediate(Value.Int(0)))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeBool true)

    [<TestMethod>]
    member this.``IsNull on null is true``() =
        let expr = Expr.UnaryOperation(UnaryOp.IsNull, Expr.Immediate(Value.Null))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeBool true)

[<TestClass>]
type BinaryOperations() =
    
    [<TestMethod>]
    member this.``Add with positive integers``() =
        let expr = Expr.BinaryOperation(BinaryOp.Add, Expr.Immediate(Value.Int(3)), Expr.Immediate(Value.Int(2)))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeInt 5)

    [<TestMethod>]
    member this.``Sub with positive integers``() =
        let expr = Expr.BinaryOperation(BinaryOp.Sub, Expr.Immediate(Value.Int(5)), Expr.Immediate(Value.Int(2)))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeInt 3)

[<TestClass>]
type CompositeExpressions() =
    
    [<TestMethod>]
    member this.``Sub followed by IsZero``() =
        let expr = 
            Expr.UnaryOperation(
                UnaryOp.IsZero, 
                Expr.BinaryOperation(BinaryOp.Sub, Expr.Immediate(Value.Int(5)), Expr.Immediate(Value.Int(5))))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeBool true)

[<TestClass>]
type LetBindings() =

    [<TestMethod>]
    member this.``Let binding and variable ref``() =
        let expr =
            Expr.LetBinding(
                [(Identifier.Variable("foo"), Expr.Immediate(Value.Int(5)))],
                Expr.BinaryOperation(
                    BinaryOp.Add,
                    Expr.Immediate(Value.Int(10)),
                    Expr.VariableRef(Identifier.Variable("foo"))))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeInt 15)
    
    [<TestMethod>]
    member this.``Multiple bindings in one let``() =
        let expr = 
            Expr.LetBinding(
                [(Identifier.Variable("foo"), Expr.Immediate(Value.Int(5)));
                 (Identifier.Variable("bar"), Expr.Immediate(Value.Int(10)))],
                Expr.BinaryOperation(
                    BinaryOp.Add,
                    Expr.VariableRef(Identifier.Variable("foo")),
                    Expr.VariableRef(Identifier.Variable("bar"))))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeInt 15)

    [<TestMethod>]
    member this.``Nested let bindings``() =
        let expr = 
            Expr.LetBinding(
                [(Identifier.Variable("foo"), Expr.Immediate(Value.Int(5)))],
                Expr.LetBinding(
                    [(Identifier.Variable("bar"), Expr.Immediate(Value.Int(10)))],
                    Expr.BinaryOperation(
                        BinaryOp.Add,
                        Expr.VariableRef(Identifier.Variable("foo")),
                        Expr.VariableRef(Identifier.Variable("bar")))))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeInt 15)

    [<TestMethod>]
    member this.``Variable shadowing``() =
        let expr = 
            Expr.LetBinding(
                [(Identifier.Variable("foo"), Expr.Immediate(Value.Int(5)))],
                Expr.LetBinding(
                    [(Identifier.Variable("foo"), Expr.Immediate(Value.Int(10)))],
                    Expr.BinaryOperation(
                        BinaryOp.Add,
                        Expr.VariableRef(Identifier.Variable("foo")),
                        Expr.Immediate(Value.Int(1)))))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeInt 11)

    [<TestMethod>]
    member this.``Expression in let binding``() =
        let expr = 
            Expr.LetBinding(
                [(Identifier.Variable("foo"), Expr.UnaryOperation(UnaryOp.Add1, Expr.Immediate(Value.Int(5))))],
                Expr.BinaryOperation(
                    BinaryOp.Add,
                    Expr.VariableRef(Identifier.Variable("foo")),
                    Expr.Immediate(Value.Int(10))))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeInt 16)

    [<TestMethod>]
    member this.``Reference to unknown variable``() =
        let expr = 
            Expr.LetBinding(
                [(Identifier.Variable("foo"), Expr.UnaryOperation(UnaryOp.Add1, Expr.Immediate(Value.Int(5))))],
                Expr.BinaryOperation(
                    BinaryOp.Add,
                    Expr.VariableRef(Identifier.Variable("bar")),
                    Expr.Immediate(Value.Int(10))))
        
        should throw typeof<System.Exception> (fun () -> compileAndRun expr |> ignore)
