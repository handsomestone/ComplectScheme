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
        | Char of char
        | Bool of bool
        | Null

    type Op =
        | Add1
        | Sub1
        | IsZero

    type Expr =
        | Immediate of Value
        | PrimitiveCall of Op * Expr

    module PrimitiveTypes =
        let encodeFixnum (x : int) =
            let shift = 2
            let tag = 0b0000
            let mask = 0b0011
            (x <<< shift) ||| tag

        let encodeChar (x : char) =
            let shift = 8
            let tag = 0b00001111
            let mask = 0b11111111
            (int(x) <<< shift) ||| tag

        let encodeBool (x : bool) =
            let shift = 7
            let tag = 0b00011111
            let mask = 0b01111111
            ((if x then 1 else 0) <<< shift) ||| tag

        let encodeNull =
            0b00101111

        let immRep (x : Value) =
            match x with
                | Int(i) -> encodeFixnum i
                | Char(c) -> encodeChar c
                | Bool(b) -> encodeBool b
                | Null -> encodeNull

    module PrimitiveOperations =
        let Add (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Add_Ovf)

        let Sub (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Sub_Ovf)

        let CompareEq (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Ceq)

    type ILEmitter(ilGen : ILGenerator) =
        let emitImmediate (ilGen : ILGenerator) (value : Value) =
            let imm = (PrimitiveTypes.immRep value)
            ilGen.Emit(OpCodes.Ldc_I4, imm)

        let convertRawToInt (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Ldc_I4_2)
            ilGen.Emit(OpCodes.Shl)

        let convertRawToChar (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Ldc_I4_8)
            ilGen.Emit(OpCodes.Shl)
            ilGen.Emit(OpCodes.Ldc_I4, 0b00001111)
            ilGen.Emit(OpCodes.Or)

        let convertRawToBool (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Ldc_I4_7)
            ilGen.Emit(OpCodes.Shl)
            ilGen.Emit(OpCodes.Ldc_I4, 0b00011111)
            ilGen.Emit(OpCodes.Or)

        let emitCall op =
            match op with
                | Op.Add1 -> 
                    emitImmediate ilGen (Value.Int(1))
                    PrimitiveOperations.Add ilGen
                | Op.Sub1 -> 
                    emitImmediate ilGen (Value.Int(1))
                    PrimitiveOperations.Sub ilGen
                | Op.IsZero ->
                    emitImmediate ilGen (Value.Int(0))
                    PrimitiveOperations.CompareEq ilGen
                    convertRawToBool ilGen

        member this.EmitExpr expr =
            let rec emitExpr expr =
                match expr with
                    | Immediate(i) -> emitImmediate ilGen i
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
    let drive (mainType : Type) args =
        let instance = Activator.CreateInstance(mainType)
        let mainMethod = mainType.GetMethod("Main")
        mainMethod.Invoke(instance, args)

    [<EntryPoint>]
    let main argv = 
        let asmInfo = { AssemblyName = "complect"; EntryPointName = "Main"; MainClassName = "MainClass"; ExecutableName = "program.exe" }
        let mainType = compile asmInfo asmInfo.ExecutableName generateMain

        let ret = drive mainType [| Array.empty<string> |]

        printfn "%A" ret
        Console.ReadLine() |> ignore
        0