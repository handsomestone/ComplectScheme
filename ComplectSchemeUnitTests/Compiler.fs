namespace ComplectScheme.UnitTests

open System.Reflection.Emit
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit
open ComplectScheme

open Compiler

[<TestClass>]
type Expressions() = 
    let (asmInfo : AssemblyInfo) = { AssemblyName = "complect"; EntryPointName = "Main"; MainClassName = "MainClass"; ExecutableName = "program.exe" }
    
    let generateMain expr (ilGen : ILGenerator) =
        let emitter = new ILEmitter(ilGen)
        emitter.EmitExpr expr
        ilGen.Emit(OpCodes.Ret)
    
    let compile expr =
        Compiler.compile asmInfo asmInfo.ExecutableName (generateMain expr)

    [<TestMethod>]
    member this.``Return immediate value``() =
        let expr = Expr.Immediate(Value.Int(23))
        let mainType = compile expr
        let ret = Compiler.drive mainType [| Array.empty<string> |]

        ret |> should equal 5  // 23 >> 2 = 5

    [<TestMethod>]
    member this.``Primitive method call``() =
        let expr = Expr.PrimitiveCall(Op.Add1, Expr.Immediate(Value.Int(23)))
        let mainType = compile expr
        let ret = Compiler.drive mainType [| Array.empty<string> |]

        ret |> should equal 6  // (23 >> 2) + 1 = 6