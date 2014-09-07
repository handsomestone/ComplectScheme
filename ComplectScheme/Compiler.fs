namespace ComplectScheme

module Compiler =
    open System
    open System.Reflection
    open System.Reflection.Emit

    type AssemblyInfo = {
        AssemblyName : string;
        ExecutableName : string;
        MainClassName : string;
        EntryPointName : string
        }

    type Value =
        | Int of int

    type Op =
        | Add1

    type Expr =
        | Immediate of Value
        | PrimitiveCall of Op * Expr
        
    let encodeFixnum x =
        let shift = 2
        let tag = 0
        let mask = 2
        x >>> shift

    let immRep (x : Value) =
        match x with
            | Int(i) ->  encodeFixnum i

    type ILEmitter(ilGen : ILGenerator) =
        let emitCall op =
            ()

        let emitImmediate (imm : int) =
            ilGen.Emit(OpCodes.Ldc_I4, imm)

        member this.EmitExpr expr =
            let rec emitExpr expr =
                match expr with
                    | Immediate(i) -> emitImmediate (immRep i)
                    | PrimitiveCall(op, v) ->
                        emitExpr v
                        emitCall op
            emitExpr expr

    let compile asmInfo outFile generateIL =
        let domain = AppDomain.CurrentDomain
        let asmName = new AssemblyName(asmInfo.AssemblyName)
        let asmBuilder = domain.DefineDynamicAssembly(asmName, AssemblyBuilderAccess.RunAndSave)
        let moduleBuilder = asmBuilder.DefineDynamicModule(asmInfo.ExecutableName, true)
        let mainTypeBuilder =
            moduleBuilder.DefineType(
                asmInfo.MainClassName,
                TypeAttributes.Public ||| TypeAttributes.Class)  // what are default attributes?
        let mainMethod = 
            mainTypeBuilder.DefineMethod(
                asmInfo.EntryPointName,
                MethodAttributes.Public ||| MethodAttributes.Static,
                typeof<int>,
                [| typeof<string>.MakeArrayType() |])

        asmBuilder.SetEntryPoint(mainMethod)

        let ilGen = mainMethod.GetILGenerator()
        do generateIL ilGen

        let mainType = mainTypeBuilder.CreateType()
        asmBuilder.Save(outFile)
        mainType

    let generateMain (ilGen : ILGenerator) =
        let emitter = new ILEmitter(ilGen)

        let expr = Expr.Immediate(Value.Int(23))
        emitter.EmitExpr(expr)

        ilGen.Emit(OpCodes.Ret)

    // Create a new instance of the main type and call the "Main" method
    let drive (mainType : Type) =
        let instance = Activator.CreateInstance(mainType)
        let mainMethod = mainType.GetMethod("Main")
        mainMethod.Invoke(instance, [| Array.empty<string> |])

    [<EntryPoint>]
    let main argv = 
        let asmInfo = { AssemblyName = "complect"; EntryPointName = "Main"; MainClassName = "MainClass"; ExecutableName = "program.exe" }
        let mainType = compile asmInfo asmInfo.ExecutableName generateMain

        let ret = drive mainType

        printfn "%A" ret
        Console.ReadLine() |> ignore
        0