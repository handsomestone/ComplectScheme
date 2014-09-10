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
        emitter.EmitExpr expr
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
        let expr = Expr.UnaryOperation(Op.Add1, Expr.Immediate(Value.Int(5)))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeInt 6)

    [<TestMethod>]
    member this.``Sub1``() =
        let expr = Expr.UnaryOperation(Op.Sub1, Expr.Immediate(Value.Int(5)))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeInt 4)

    [<TestMethod>]
    member this.``IsZero on 0 is true``() =
        let expr = Expr.UnaryOperation(Op.IsZero, Expr.Immediate(Value.Int(0)))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeBool true)

    [<TestMethod>]
    member this.``IsNull on null is true``() =
        let expr = Expr.UnaryOperation(Op.IsNull, Expr.Immediate(Value.Null))
        let ret = compileAndRun expr

        ret |> should equal (PrimitiveTypes.encodeBool true)
