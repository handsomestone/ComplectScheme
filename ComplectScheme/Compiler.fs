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
        type TypeInfo = { Tag : int; Mask : int }
        module TypeInfos =
            let Int = { Tag = 0b0000; Mask = 0b0011 }
            let Char = { Tag = 0b00001111; Mask = 0b11111111 }
            let Bool = { Tag = 0b00011111; Mask = 0b01111111 }
            let Null = { Tag = 0b00101111; Mask = 0b11111111 }

        let encodeInt (x : int) =
            (x <<< 2) ||| TypeInfos.Int.Tag

        let (|Int|_|) x =
            let { Tag = tag; Mask = mask} = TypeInfos.Int
            if (x &&& mask) = tag then Some(Value.Int(x >>> 2))
            else None

        let encodeChar (x : char) =
            (int(x) <<< 8) ||| TypeInfos.Char.Tag

        let (|Char|_|) x =
            let { Tag = tag; Mask = mask} = TypeInfos.Char
            if (x &&& mask) = tag then Some(Value.Char(char((x >>> 8) &&& 0xff)))
            else None

        let encodeBool (x : bool) =
            ((if x then 1 else 0) <<< 7) ||| TypeInfos.Bool.Tag

        let (|Bool|_|) x =
            let { Tag = tag; Mask = mask} = TypeInfos.Bool
            if (x &&& mask) = tag then Some(Value.Bool(((x >>> 8) &&& 0b1) = 0b1))
            else None

        let encodeNull =
            TypeInfos.Null.Tag

        let (|Null|_|) x =
            let { Tag = tag; Mask = mask} = TypeInfos.Null
            if (x &&& mask) = tag then Some(Value.Null)
            else None

        let encodeValue (x : Value) =
            match x with
                | Value.Int i -> encodeInt i
                | Value.Char c -> encodeChar c
                | Value.Bool b -> encodeBool b
                | Value.Null -> encodeNull

        let decodeValue (x : int) =
            match x with  // active patterns, not Value union cases
                | Int i -> i
                | Char c -> c
                | Bool b -> b
                | Null n -> n
                | _ -> failwith "Unknown type"

        let convertRawToInt (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Ldc_I4_2)
            ilGen.Emit(OpCodes.Shl)
            ilGen.Emit(OpCodes.Ldc_I4, TypeInfos.Int.Tag)
            ilGen.Emit(OpCodes.Or)

        let convertRawToChar (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Ldc_I4_8)
            ilGen.Emit(OpCodes.Shl)
            ilGen.Emit(OpCodes.Ldc_I4, TypeInfos.Char.Tag)
            ilGen.Emit(OpCodes.Or)

        let convertRawToBool (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Ldc_I4_7)
            ilGen.Emit(OpCodes.Shl)
            ilGen.Emit(OpCodes.Ldc_I4, TypeInfos.Bool.Tag)
            ilGen.Emit(OpCodes.Or)

    module PrimitiveOperations =
        let Add (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Add_Ovf)

        let Sub (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Sub_Ovf)

        let CompareEq (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Ceq)

    type ILEmitter(ilGen : ILGenerator) =
        let emitImmediate (ilGen : ILGenerator) (value : Value) =
            let imm = (PrimitiveTypes.encodeValue value)
            ilGen.Emit(OpCodes.Ldc_I4, imm)

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
                    PrimitiveTypes.convertRawToBool ilGen

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