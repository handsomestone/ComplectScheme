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
    type TypedIdentifier = Identifier * Type
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
        | VariableRef of Identifier  // needs qualifiers?
        | UnaryOperation of UnaryOp * Expr
        | BinaryOperation of BinaryOp * Expr * Expr
        | LetBinding of Binding list * Expr
        | Conditional of Expr * Expr * Expr
        | FunctionCall of Expr * Binding list
        | Lambda of Identifier list * Identifier list * Expr
        | Closure of Identifier * TypedIdentifier list
        | Assign of Identifier * Expr
        | Sequence of Expr list
    and Binding = Identifier * Expr

    type CtorDef = {
        Builder : ConstructorBuilder option;
        Body : Expr;
        Parameters : TypedIdentifier list
        }
        with
        member this.GetBuilder() =
            match this.Builder with
                | Some(builder) -> builder
                | None -> failwith "Failed to get builder for ctor" 

    type MethodDef = {
        Builder : MethodBuilder option;
        Name : string;
        Body : Expr;
        ReturnType : Type option;
        Parameters : TypedIdentifier list
        IsStatic : bool;
        }
        with
        member this.GetBuilder() =
            match this.Builder with
                | Some(builder) -> builder
                | None -> failwithf "Failed to get builder for method %s" this.Name 

    type FieldDef = {
        Builder : FieldBuilder option;
        Name : string;
        Type : Type;
        }
        with
        member this.GetBuilder() =
            match this.Builder with
                | Some(builder) -> builder
                | None -> failwithf "Failed to get builder for field %s" this.Name 

    type TypeDef = {
        Builder : TypeBuilder option;
        Name : string;
        Functions : MethodDef list;
        Ctors : CtorDef list;
        NestedTypes : TypeDef list;
        IsNested : bool;
        Fields : FieldDef list;
        // InheritsFrom?
        }
        with
        member this.GetBuilder() =
            match this.Builder with
                | Some(builder) -> builder
                | None -> failwithf "Failed to get builder for type %s" this.Name 

    let someOrNull =
        function
            | Some (t) -> t
            | None -> null

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

    type RewriterWrapper = (Expr * TypeDef list)

    type RewriterBuilder() =
        member this.Bind(x : RewriterWrapper, f : (Expr -> RewriterWrapper)) : RewriterWrapper =
            let (expr, types) = x
            let (expr', types') = f expr
            (expr', List.append types types')

        member this.Return(x) : RewriterWrapper =
            (x, [])

    type LambdaRewriter(ctx : CompilerContext) =
        let createLambdaType (formalParams : TypedIdentifier list) (capturedParams : TypedIdentifier list) expr =
            let invokeMethod : MethodDef = {
                Name = "Invoke";
                Builder = None;
                Body = expr;  // TODO -- rewrite storage locs?
                ReturnType = Some(typeof<int>);  // ???
                Parameters = formalParams;
                IsStatic = false;
            }

            let capturedFields = capturedParams |> List.map (fun (id, t) -> { Name = id; Type = t; Builder = None })

            let renamedCtorParams = capturedParams |> List.map(fun (id, t) -> ("_" + id, t))

            let ctorExpr = 
                Expr.Sequence(
                    capturedParams |> List.map (fun (id, t) ->
                        Expr.Assign(id, Expr.VariableRef("_" + id))
                        ))

            let ctor : CtorDef = {
                Builder = None;
                Body = ctorExpr;
                Parameters = renamedCtorParams
            }

            let lambdaType : TypeDef = {
                Name = (sprintf "lambda%i" (ctx.SymGen.GetNewSymbol()));
                Builder = None
                Functions = [ invokeMethod ];
                Ctors = [ ctor ];
                NestedTypes = [];
                IsNested = true;
                Fields = capturedFields;
                }

            lambdaType

        let defaultTypes (ids : Identifier list) =
            ids |> List.map (fun id -> (id, typeof<int>))

        member this.Rewriter (wrapper : RewriterWrapper) =
            let (expr, types) = wrapper
            match expr with
                | Lambda(formalParams, capturedParams, e) ->
                    let formalParams' = (defaultTypes formalParams)
                    let capturedParams' = (defaultTypes capturedParams)
                    let closure = createLambdaType formalParams' capturedParams' e
                    // TODO -- need to replace the Lambda expr with a new instance of lambda
                    (Expr.Closure(closure.Name, capturedParams'), closure :: types)
                | other -> (other, types)

    type Rewriter() =
        member this.RewriteMethod (rewriter : (RewriterWrapper -> RewriterWrapper)) (methodDef : MethodDef) =
            let rw = new RewriterBuilder()
            let rec rewriteExpr expr =
                let wrapper' =
                    match expr with
                        | Immediate(v) -> 
                            rw {
                                return Immediate(v)
                            }
                        | VariableRef(ref) -> 
                            rw {
                                return VariableRef(ref)
                            }
                        | UnaryOperation(op, e) -> 
                            rw {
                                let! e' = rewriteExpr e
                                return UnaryOperation(op, e')
                            }
                        | BinaryOperation(op, e1, e2) -> 
                            rw {
                                let! e1' = rewriteExpr e1
                                let! e2' = rewriteExpr e2
                                return BinaryOperation(op, e1', e2')
                            }
                        | LetBinding(b, e) ->
                            rw {
                                let! e' = rewriteExpr e
                                return LetBinding(b, e')
                            }
                        | Conditional(e1, e2, e3) -> 
                            rw {
                                let! e1' = rewriteExpr e1
                                let! e2' = rewriteExpr e2
                                let! e3' = rewriteExpr e3
                                return Conditional(e1', e2', e3')
                            }
                        | FunctionCall (e, b) -> 
                            rw {
                                let! e' = rewriteExpr e
                                return FunctionCall(e', b)
                            }
                        | Lambda(p1, p2, e) -> 
                            rw {
                                let! e' = rewriteExpr e
                                return Lambda(p1, p2, e')
                            }
                        | Closure(id, p) ->
                            rw {
                                return Closure(id, p)
                            }
                        | Assign(id, e) -> 
                            rw {
                                let! e' = rewriteExpr e
                                return Assign(id, e')
                            }
                        | Sequence(es) -> 
                            rw { 
                                let wrappers = es |> List.map rewriteExpr
                                let es' = wrappers |> List.map fst
                                return Sequence(es')
                            }
                rewriter wrapper'
            let (rewritten, types') = rewriteExpr methodDef.Body

            ({ methodDef with Body = rewritten }, types')

        member this.RewriteType rewriter (typeDef : TypeDef) =
            let (methods', types') = 
                List.foldBack (fun m (ms, ts) -> 
                    let (rewritten, newTypes) = this.RewriteMethod rewriter m
                    (rewritten :: ms, List.append newTypes ts)
                    ) typeDef.Functions ([], [])
            { typeDef with Functions = methods'; NestedTypes = (List.append typeDef.NestedTypes types') }
        
    type MethodCompiler(ilGen : ILGenerator, typeDef : TypeDef) =
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

        let emitLocalVariableLoad (ref : int) =
            ilGen.Emit(OpCodes.Ldloc, ref)
            
        let emitLocalVariableStore (ref : int) =
            ilGen.Emit(OpCodes.Stloc, ref)

        let emitFieldLoad (ref : FieldInfo) =
            ilGen.Emit(OpCodes.Ldfld, ref)
            
        let emitFieldStore (ref : FieldInfo) =
            ilGen.Emit(OpCodes.Stfld, ref)

        let emitArgumentLoad (ref : int) =
            ilGen.Emit(OpCodes.Ldarg, ref)

        let emitArgumentStore (ref : int) =
            ilGen.Emit(OpCodes.Starg, ref)

        let rec emitStorageLoad (stg : StorageLoc) =
            match stg with
                | LocalStorage local -> emitLocalVariableLoad local
                | ArgumentStorage arg -> emitArgumentLoad arg
                | FieldStorage (stg, fi) -> 
                    emitStorageLoad stg
                    emitFieldLoad fi

        let rec emitStorageStore (stg : StorageLoc) emitValue  =
            match stg with
                | LocalStorage local ->
                    emitValue()
                    emitLocalVariableStore local
                | ArgumentStorage arg -> failwith "Can't store to argument storage" //emitArgumentStore arg
                | FieldStorage (stg, fi) -> 
                    emitStorageLoad stg
                    emitValue()
                    emitFieldStore fi

        let emitVariableRef ref (env : Env) =
            match env.FindIdentifier ref with
                | Some stg -> 
                    emitStorageLoad stg
                | None -> failwithf "Unable to find binding for identifier %s" ref

        let emitVariableAssignment ref (env : Env) emitValue =
            match env.FindIdentifier ref with
                | Some stg ->
                    emitStorageStore stg emitValue
                | None -> failwithf "Unable to find binding for identifier %s" ref
            
        let storeLocalVariable (binding : Binding) : BindingRef =
            let (id, expr) = binding
            let localBuilder = ilGen.DeclareLocal(typeof<int>)
            localBuilder.SetLocalSymInfo(id)
            let stgLoc = LocalStorage(localBuilder.LocalIndex)
            ilGen.Emit(OpCodes.Stloc, localBuilder)
            (id, stgLoc)

        let emitNewObj (ctor : ConstructorInfo) =
            ilGen.Emit(OpCodes.Newobj, ctor)
            
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
                        let invokeMethod = typeof<Func<int,int>>.GetMethod("Invoke")
                        ilGen.Emit(OpCodes.Callvirt, invokeMethod)
                    | Lambda(formalParams, capturedParams, e) ->
                        printf "wtf"
//                        let (lambdaCtor, invokeMethod) = createLambdaType formalParams capturedParams e
//                        emitNewObj lambdaCtor capturedParams
//                        ilGen.Emit(OpCodes.Ldftn, invokeMethod)
//                        ilGen.Emit(OpCodes.Newobj, typeof<Func<int, int>>.GetConstructor([| typeof<obj>; typeof<IntPtr> |]))
//                        ()
                    | Closure(typeId, args) ->
                        let lambdaType = 
                            match typeDef.NestedTypes |> List.tryFind (fun t -> t.Name = typeId) with
                                | Some(t) -> t
                                | None -> failwithf "Unable to find referenced lambda type %s" typeId
                        let ctor = lambdaType.Ctors |> Seq.head // (args |> List.map snd |> List.toArray)
                        args |> List.iter (fun (arg, argType) ->
                                emitVariableRef arg env
                            )
                        emitNewObj (ctor.GetBuilder())
                        let invokeMethod = lambdaType.Functions |> List.find (fun f -> f.Name = "Invoke")
                        ilGen.Emit(OpCodes.Ldftn, invokeMethod.GetBuilder())
                        ilGen.Emit(OpCodes.Newobj, typeof<Func<int, int>>.GetConstructor([| typeof<obj>; typeof<IntPtr> |]))
                    | Assign(id, e) ->
                        emitVariableAssignment id env (fun () -> emitExpr e env)
                    | Sequence(exprs) ->
                        exprs |> List.iter (fun e -> emitExpr e env)
            emitExpr expr env
            ilGen.Emit(OpCodes.Ret)

        member this.CompileCtor expr env =
            ilGen.Emit(OpCodes.Ldarg_0)
            ilGen.Emit(
                OpCodes.Call,
                (typeof<obj>).GetConstructor([||]))
            this.CompileMethod expr env

    let compileMethod (typeDef : TypeDef) (env : Env) (methodDef : MethodDef) =
        let typeBuilder = typeDef.GetBuilder()

        let methodAttrs =
            if methodDef.IsStatic then
               MethodAttributes.Public ||| MethodAttributes.Static
            else
                MethodAttributes.Public

        let methodBuilder = 
            typeBuilder.DefineMethod(
                methodDef.Name,
                methodAttrs,
                (someOrNull methodDef.ReturnType),
                methodDef.Parameters |> List.map snd |> List.toArray)

        let mcompiler = new MethodCompiler(methodBuilder.GetILGenerator(), typeDef)

        let argBindings = methodDef.Parameters |> List.mapi (fun i (id, t) -> (id, StorageLoc.ArgumentStorage(i + 1)))
        let env2 = new Env(Some(env), Some(argBindings))

        mcompiler.CompileMethod methodDef.Body env2

        { methodDef with Builder = Some(methodBuilder) }

    let compileCtor (typeDef : TypeDef) (env : Env) (ctorDef : CtorDef) =
        let typeBuilder = typeDef.GetBuilder()
            
        let ctorBuilder =
            typeBuilder.DefineConstructor(
                MethodAttributes.Public,
                CallingConventions.HasThis,
                ctorDef.Parameters |> List.map snd |> List.toArray)
        
        let mcompiler = new MethodCompiler(ctorBuilder.GetILGenerator(), typeDef)
        
        let argBindings = ctorDef.Parameters |> List.mapi (fun i (id, t) -> (id, StorageLoc.ArgumentStorage(i + 1)))
        let env2 = new Env(Some(env), Some(argBindings))

        mcompiler.CompileCtor ctorDef.Body env2

        { ctorDef with Builder = Some(ctorBuilder) }

    let compileField (typeBuilder : TypeBuilder) (fieldDef : FieldDef) =
        let fieldBuilder =
            typeBuilder.DefineField(
                fieldDef.Name,
                fieldDef.Type,
                FieldAttributes.Public)

        { fieldDef with Builder = Some(fieldBuilder) }

    let compileTypeMembers (typeDef : TypeDef) =
        let typeBuilder = typeDef.GetBuilder()

        let fieldDefs = typeDef.Fields |> List.map (compileField typeBuilder)
        let fieldBindings = 
            fieldDefs
            |> List.map (fun fieldDef -> 
                let fieldBuilder = fieldDef.GetBuilder()
                (fieldDef.Name, StorageLoc.FieldStorage(StorageLoc.ArgumentStorage(0), fieldBuilder))
                )

        let env = new Env(None, Some(fieldBindings))
        { typeDef with
            Functions = typeDef.Functions |> List.map (compileMethod typeDef env)
            Ctors = typeDef.Ctors |> List.map (compileCtor typeDef env)
        }
        
    let compileNestedType (outerTypeBuilder : TypeBuilder) (typeDef : TypeDef) =
        let innerTypeBuilder =
            outerTypeBuilder.DefineNestedType(
                typeDef.Name,
                TypeAttributes.Class ||| TypeAttributes.NestedPublic)

        let typeDef' =
            { typeDef with
                Builder = Some(innerTypeBuilder);
            }

        compileTypeMembers typeDef'

    let compileType (moduleBuilder : ModuleBuilder) (ctx : CompilerContext) (typeDef : TypeDef) =
        let typeBuilder =
            moduleBuilder.DefineType(
                typeDef.Name,
                TypeAttributes.Class ||| TypeAttributes.Public)

        let typeDef' = 
            { typeDef with
                Builder = Some(typeBuilder);
                NestedTypes = typeDef.NestedTypes |> List.map (fun inner -> compileNestedType typeBuilder inner)
            }

        let typeDef' = compileTypeMembers typeDef'
        // Note the outer type needs to be "created" before the nested types
        let createdType = typeBuilder.CreateType()

        typeDef'.NestedTypes |> List.iter (fun t -> 
            match t.Builder with
                | Some(builder) -> builder.CreateType() |> ignore
                | None -> failwithf "Failed to finalize nested type %s" t.Name)
        
        createdType
        
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

        let rewriter = new Rewriter()
        let lambdaRewriter = new LambdaRewriter(ctx)
        let expandedTypes = typeDefs |> List.map (rewriter.RewriteType lambdaRewriter.Rewriter)
        expandedTypes |> List.iter (fun t -> printf "%A" t)

        let createdTypes = expandedTypes |> List.map (compileType moduleBuilder ctx)
        
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
            ExecutableName = "program.exe";
        }
        let mainFunctionInfo = {
            Name = "Main";
            Body = mainExpr;
            ReturnType = Some(typeof<int>);
            Parameters = [ ("args", typeof<string>.MakeArrayType()) ];
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

        let mainType = build asmInfo mainTypeInfo
        let ret = drive mainType [| Array.empty<string> |]

        printfn "%A" ret
        Console.ReadLine() |> ignore
        0