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
    member this.``Int immediate value``() =
        let expr = Expr.Immediate(Value.Int(5))
        let mainType = compile expr
        let ret = Compiler.drive mainType [| Array.empty<string> |]

        ret |> should equal 20  // 5 <<< 2 = 20

    [<TestMethod>]
    member this.``Char immediate value``() =
        let expr = Expr.Immediate(Value.Char('a'))
        let mainType = compile expr
        let ret = Compiler.drive mainType [| Array.empty<string> |]

        ret |> should equal 24847  // 'a' = 97; (97 <<< 8) ||| 0b00001111 =  24847

    [<TestMethod>]
    member this.``Bool immediate value``() =
        let expr = Expr.Immediate(Value.Bool(true))
        let mainType = compile expr
        let ret = Compiler.drive mainType [| Array.empty<string> |]

        ret |> should equal 159  // true = 1; (1 <<< 7) ||| 0b0011111 =  159 

    [<TestMethod>]
    member this.``Add1``() =
        let expr = Expr.PrimitiveCall(Op.Add1, Expr.Immediate(Value.Int(5)))
        let mainType = compile expr
        let ret = Compiler.drive mainType [| Array.empty<string> |]

        ret |> should equal 24  // (5 <<< 2) + (1 <<< 2) = 24

    [<TestMethod>]
    member this.``Sub1``() =
        let expr = Expr.PrimitiveCall(Op.Sub1, Expr.Immediate(Value.Int(5)))
        let mainType = compile expr
        let ret = Compiler.drive mainType [| Array.empty<string> |]

        ret |> should equal 16  // (5 <<< 2) - (1 <<< 2) = 16
