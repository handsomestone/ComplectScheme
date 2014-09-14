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

    type CompilerInfo = {
        Domain : AppDomain;
        AsmName : AssemblyName;
        AsmBuilder : AssemblyBuilder;
        ModuleBuilder : ModuleBuilder;
        }

    type SymbolGenerator() =
        let xs = (Seq.initInfinite (fun i -> i)).GetEnumerator()
        member this.GetNewSymbol() : int =
            xs.MoveNext() |> ignore
            xs.Current

    type CompilerContext = {
        SymGen : SymbolGenerator;
        }

    type Value =
        | Int of int
        | Char of char
        | Bool of bool
        | Null

    type Identifier = string
    //type FunctionId = string * Type

    type StorageLoc =
        | LocalStorage of int
        | ArgumentStorage of int
        | FieldStorage of StorageLoc * FieldInfo

    type BindingRef = Identifier * StorageLoc

    type UnaryOp =
        | Add1
        | Sub1
        | IsZero
        | IsNull

    type BinaryOp =
        | Add
        | Sub

    type Expr =
        | Immediate of Value
        | VariableRef of Identifier
        | UnaryOperation of UnaryOp * Expr
        | BinaryOperation of BinaryOp * Expr * Expr
        | LetBinding of Binding list * Expr
        | Conditional of Expr * Expr * Expr
        | FunctionCall of Expr * Binding list
        | Lambda of Identifier list * Identifier list * Expr
    and Binding = Identifier * Expr

    type MethodDef = {
        Name : string;
        Body : Expr;
        ReturnType : Type;
        ParameterTypes : Type list
        }

    type TypeDef = {
        Name : string;
        Functions : MethodDef list;
        }

    type Env(env : Env option, bindings : BindingRef list option) =
        let map = new Map<Identifier, StorageLoc>(match bindings with Some(b) -> (Seq.ofList b) | None -> Seq.empty)

        member this.FindIdentifier id =
            match (map |> Map.tryFind id), env with
                | Some stg, _-> Some stg
                | None, Some e -> e.FindIdentifier id
                | None, None -> None

    module PrimitiveTypes =
        type TypeInfo = { Tag : int; Mask : int }
        module TypeInfos =
            let Int = { Tag = 0b0000; Mask = 0b0011 }
            let Char = { Tag = 0b00001111; Mask = 0b11111111 }
            let Bool = { Tag = 0b00011111; Mask = 0b01111111 }
            let Null = { Tag = 0b00101111; Mask = 0b11111111 }

        let encodeInt (x : int) =
            (x <<< 2) ||| TypeInfos.Int.Tag

        let (|IsInt|_|) x =
            let { Tag = tag; Mask = mask} = TypeInfos.Int
            if (x &&& mask) = tag then Some(Value.Int(x >>> 2))
            else None

        let encodeChar (x : char) =
            (int(x) <<< 8) ||| TypeInfos.Char.Tag

        let (|IsChar|_|) x =
            let { Tag = tag; Mask = mask} = TypeInfos.Char
            if (x &&& mask) = tag then Some(Value.Char(char((x >>> 8) &&& 0xff)))
            else None

        let encodeBool (x : bool) =
            ((if x then 1 else 0) <<< 7) ||| TypeInfos.Bool.Tag

        let (|IsBool|_|) x =
            let { Tag = tag; Mask = mask} = TypeInfos.Bool
            if (x &&& mask) = tag then Some(Value.Bool(((x >>> 8) &&& 0b1) = 0b1))
            else None

        let encodeNull =
            TypeInfos.Null.Tag

        let (|IsNull|_|) x =
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
            match x with 
                | IsInt i -> i
                | IsChar c -> c
                | IsBool b -> b
                | IsNull n -> n
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

        let getNativeTypeForValue value =
            typeof<int>  // only ints for now, maybe objects later (e.g. functions, etc.)

    module PrimitiveOperations =
        let Add (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Add_Ovf)

        let Sub (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Sub_Ovf)

        let CompareEq (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Ceq)
        
    type MethodCompiler(methodBuilder : MethodBuilder, typeBuilder : TypeBuilder, ctx : CompilerContext) =
        let ilGen = methodBuilder.GetILGenerator()
            
        let emitValue (value : Value) =
            let imm = (PrimitiveTypes.encodeValue value)
            ilGen.Emit(OpCodes.Ldc_I4, imm)

        let emitUnaryOp op =
            match op with
                | UnaryOp.Add1 -> 
                    emitValue (Value.Int(1))
                    PrimitiveOperations.Add ilGen
                | UnaryOp.Sub1 -> 
                    emitValue (Value.Int(1))
                    PrimitiveOperations.Sub ilGen
                | UnaryOp.IsZero ->
                    emitValue (Value.Int(0))
                    PrimitiveOperations.CompareEq ilGen
                    PrimitiveTypes.convertRawToBool ilGen
                | UnaryOp.IsNull ->
                    emitValue (Value.Null)
                    PrimitiveOperations.CompareEq ilGen
                    PrimitiveTypes.convertRawToBool ilGen

        let emitBinaryOp op =
            match op with
                | BinaryOp.Add ->
                    PrimitiveOperations.Add ilGen
                | BinaryOp.Sub ->
                    PrimitiveOperations.Sub ilGen

        let emitLocalVariableRef (ref : int) =
            ilGen.Emit(OpCodes.Ldloc, ref)

        let emitFieldVariableRef (ref : FieldInfo) =
            ilGen.Emit(OpCodes.Ldfld, ref)

        let emitArgumentRef (ref : int) =
            ilGen.Emit(OpCodes.Ldarg, ref)

        let emitVariableRef ref (env : Env) =
            match env.FindIdentifier ref with
                | Some stg -> 
                    match stg with
                        | LocalStorage local -> emitLocalVariableRef local
                        | ArgumentStorage arg -> emitArgumentRef arg
                | None -> failwithf "Unable to find binding for identifier %s" ref
            
        let storeLocalVariable (binding : Binding) : BindingRef =
            let (id, expr) = binding
            let localBuilder = ilGen.DeclareLocal(typeof<int>)
            localBuilder.SetLocalSymInfo(id)
            let stgLoc = LocalStorage(localBuilder.LocalIndex)
            ilGen.Emit(OpCodes.Stloc, localBuilder)
            (id, stgLoc)

        let emitFunctionCall() =
            let invokeMethod = typeof<Delegate>.GetMethod("Invoke")
            ilGen.Emit(OpCodes.Callvirt, invokeMethod)

        let createLambdaType (formalParams : Identifier list) (capturedParams : Identifier list) expr =
            let lambdaType = 
                typeBuilder.DefineNestedType(
                    (sprintf "lambda%i" (ctx.SymGen.GetNewSymbol())),
                    TypeAttributes.Class,
                    typeof<Delegate>)

            let methodBuilder = 
                lambdaType.DefineMethod(
                    "Invoke", 
                    MethodAttributes.Public,
                    typeof<int>,
                    formalParams |> List.map (fun t -> typeof<int>) |> List.toArray)

            let capturedFields =
                capturedParams 
                |> List.map (fun id -> 
                    let fieldBuilder = typeBuilder.DefineField(id, typeof<int>, FieldAttributes.Public)
                    (id, StorageLoc.FieldStorage(StorageLoc.ArgumentStorage(0), fieldBuilder))
                    )

            let functionParams =
                formalParams
                |> List.mapi (fun i id -> (id, StorageLoc.ArgumentStorage(i + 1)))

            let methodCompiler = new MethodCompiler(methodBuilder, typeBuilder, ctx)

            let env = new Env(None, Some(capturedFields))
            methodCompiler.CompileMethod expr env

            // compile constructor method
            // emit constructor call w/ captured fields
            ()
            
        member this.CompileMethod expr env =
            let rec emitExpr expr env =
                match expr with
                    | Immediate(i) -> emitValue i
                    | UnaryOperation(op, e) ->
                        emitExpr e env
                        emitUnaryOp op
                    | BinaryOperation(op, e1, e2) ->
                        emitExpr e1 env
                        emitExpr e2 env
                        emitBinaryOp op
                    | VariableRef(ref) ->
                        emitVariableRef ref env
                    | LetBinding(bindings, e) ->
                        let bindingRefs = 
                            bindings 
                            |> List.map (fun binding -> 
                                let (_, expr) = binding
                                emitExpr expr env
                                storeLocalVariable binding
                                )
                        emitExpr e (new Env(Some(env), Some(bindingRefs)))
                    | Conditional(test, e1, e2) ->
                        let l0 = ilGen.DefineLabel()
                        let l1 = ilGen.DefineLabel()
                        emitExpr test env
                        emitValue (Value.Bool(false))
                        ilGen.Emit(OpCodes.Beq, l0)
                        emitExpr e1 env
                        ilGen.Emit(OpCodes.Br, l1)
                        ilGen.MarkLabel(l0)
                        emitExpr e2 env
                        ilGen.MarkLabel(l1)
                    | FunctionCall(e, bindings) ->
                        emitExpr e env
                        bindings |> List.iter (fun binding -> 
                            // TODO -- these should be ordered against the function args, by name?
                            let (id, expr) = binding
                            emitExpr expr env
                            )
                        emitFunctionCall()
                    | Lambda(formalParams, capturedParams, e) ->
                        let lambdaType = createLambdaType formalParams capturedParams e
                        emitNewObj lambdaType
            emitExpr expr env
            ilGen.Emit(OpCodes.Ret)

    let compileMethod (typeBuilder : TypeBuilder) (ctx : CompilerContext) (methodDef : MethodDef) =
        let methodBuilder = 
            typeBuilder.DefineMethod(
                methodDef.Name,
                MethodAttributes.Public ||| MethodAttributes.Static,
                methodDef.ReturnType,
                List.toArray methodDef.ParameterTypes)

        let mcompiler = new MethodCompiler(methodBuilder, typeBuilder, ctx)
        let env = new Env(None, None)
        mcompiler.CompileMethod methodDef.Body env

    let compileType (moduleBuilder : ModuleBuilder) (ctx : CompilerContext) (typeDef : TypeDef) =
        let typeBuilder =
            moduleBuilder.DefineType(
                typeDef.Name,
                TypeAttributes.Public ||| TypeAttributes.Class)  // what are default attributes?

        typeDef.Functions |> List.iter (compileMethod typeBuilder ctx)
        typeBuilder.CreateType()

    let compile asmInfo outFile (typeDefs : TypeDef list) (entryPoint : string) (ctx : CompilerContext) =
        let domain = AppDomain.CurrentDomain
        let asmName = new AssemblyName(asmInfo.AssemblyName)
        let asmBuilder = domain.DefineDynamicAssembly(asmName, AssemblyBuilderAccess.RunAndSave)
        let moduleBuilder = asmBuilder.DefineDynamicModule(asmInfo.ExecutableName, true)

        let compilerInfo = {
            Domain = domain;
            AsmName = asmName;
            AsmBuilder = asmBuilder;
            ModuleBuilder = moduleBuilder;
            }

        let createdTypes = typeDefs |> List.map (compileType moduleBuilder ctx)
        
        let mainMethod = 
            createdTypes 
            |> List.choose (fun f ->
                match f.GetMethod(entryPoint) with 
                    | null -> None 
                    | m -> Some(m))
            |> Seq.exactlyOne

        asmBuilder.SetEntryPoint(mainMethod)
        asmBuilder.Save(outFile)
        createdTypes

    let mainExpr =
        let expr =
            Expr.LetBinding(
                [("foo", Expr.Immediate(Value.Int(5)))],
                Expr.BinaryOperation(
                    BinaryOp.Add,
                    Expr.Immediate(Value.Int(10)),
                    Expr.VariableRef("foo")))
        expr

    let drive (mainType : Type) args =
        let instance = Activator.CreateInstance(mainType)
        let mainMethod = mainType.GetMethod("Main")
        mainMethod.Invoke(instance, args)

    let build (asmInfo : AssemblyInfo)  (mainTypeDef : TypeDef) =
        let ctx = { SymGen = new SymbolGenerator() }
        let createdTypes = compile asmInfo asmInfo.ExecutableName [ mainTypeDef ] asmInfo.EntryPointName ctx
        let mainType =
            createdTypes
            |> List.find (fun f -> f.Name = asmInfo.MainClassName)
        mainType

    [<EntryPoint>]
    let main argv = 
        let asmInfo = {
            AssemblyName = "complect";
            EntryPointName = "Main";
            MainClassName = "MainClass";
            ExecutableName = "program.exe"
        }
        let mainFunctionInfo = {
            Name = "Main";
            Body = mainExpr;
            ReturnType = typeof<int>;
            ParameterTypes = [ typeof<string>.MakeArrayType() ];
        }
        let mainTypeInfo = {
            Name  = "MainClass";
            Functions = [ mainFunctionInfo ]
        }

        let mainType = build asmInfo mainTypeInfo
        let ret = drive mainType [| Array.empty<string> |]

        printfn "%A" ret
        Console.ReadLine() |> ignore
        0