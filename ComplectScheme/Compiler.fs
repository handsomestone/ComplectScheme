namespace ComplectScheme

module Compiler =
    open System
    open System.Reflection
    open System.Reflection.Emit

    open Util
    open Assembly
    open Symbols
    open Expressions
    open Metadata
    open Rewriting

    type Env(env : Env option, bindings : BindingRef list option) =
        member private this.map = new Map<Identifier, StorageLoc>(match bindings with Some(b) -> (Seq.ofList b) | None -> Seq.empty)

        member private this.merge (other : BindingRef list) =
            match bindings with
                | Some(b) -> List.append other b
                | None -> other

        member this.FindIdentifier id =
            match (this.map |> Map.tryFind id), env with
                | Some stg, _-> Some stg
                | None, Some e -> e.FindIdentifier id
                | None, None -> None

        member this.MergeWith (other : Env) =
            let combined =
                match bindings with
                    | Some(b) -> other.merge b
                    | None -> other.merge []
            new Env(env, Some(combined))

    type ExpressionCompiler(ilGen : ILGenerator, typeDef : TypeDef) =
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
            
        member this.CompileExpression expr env =
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
                        failwithf "Encountered unexpected Lambda expression without closure"
                    | Closure(typeId, args) ->
                        let lambdaType = 
                            match typeDef.NestedTypes |> List.tryFind (fun t -> t.Name = typeId) with
                                | Some(t) -> t
                                | None -> failwithf "Unable to find referenced lambda type %s" typeId
                        let ctor = lambdaType.Ctors |> Seq.exactlyOne
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
            this.CompileExpression expr env


    type TypeCompilerWrapper = TypeDef * Env
    
    type TypeCompilerBuilder(wrapper) =
        member this.Bind(x, f) =
            f x wrapper
        
        member this.Return(x) = x
         
    (* TODO -- need to figure out exactly the sequence of declaring / defining types and members here should be
     * e.g.
     * 1) Declare outer type
     * 2) Declare nested types
     * 3) Define outer fields
     * 4) Define outer methods
     * 5) Define nested fields
     * 6) Define nested methods
     *) 
    module TypeCompiler =
        let DefineField (typeDef : TypeDef) (fieldDef : FieldDef) : FieldDef =
            let fieldBuilder =
                typeDef.GetBuilder().DefineField(
                    fieldDef.Name,
                    fieldDef.Type,
                    FieldAttributes.Public)

            { fieldDef with Builder = Some(fieldBuilder) }

        let DefineMethod (typeDef : TypeDef, env : Env) (methodDef : MethodDef) =
            let methodAttrs =
                if methodDef.IsStatic then
                   MethodAttributes.Public ||| MethodAttributes.Static
                else
                    MethodAttributes.Public

            let methodBuilder = 
                typeDef.GetBuilder().DefineMethod(
                    methodDef.Name,
                    methodAttrs,
                    (someOrNull methodDef.ReturnType),
                    methodDef.Parameters |> List.map snd |> List.toArray)

            let exprCompiler = new ExpressionCompiler(methodBuilder.GetILGenerator(), typeDef)

            let argBindings = methodDef.Parameters |> List.mapi (fun i (id, _) -> (id, StorageLoc.ArgumentStorage(i + 1)))
            let env' = new Env(Some(env), Some(argBindings))

            exprCompiler.CompileExpression methodDef.Body env'

            { methodDef with Builder = Some(methodBuilder) }

        let DefineMethods (typeDef : TypeDef, env : Env) =
            let typeDef' =
                { typeDef with
                    Functions = typeDef.Functions |> List.map (DefineMethod (typeDef, env))
                }

            (typeDef', env)

        let DefineCtor (typeDef : TypeDef, env : Env) (ctorDef : CtorDef) =
            let ctorBuilder =
                typeDef.GetBuilder().DefineConstructor(
                    MethodAttributes.Public,
                    CallingConventions.HasThis,
                    ctorDef.Parameters |> List.map snd |> List.toArray)
        
            let exprCompiler = new ExpressionCompiler(ctorBuilder.GetILGenerator(), typeDef)
        
            let argBindings = ctorDef.Parameters |> List.mapi (fun i (id, _) -> (id, StorageLoc.ArgumentStorage(i + 1)))
            let env' = new Env(Some(env), Some(argBindings))

            exprCompiler.CompileCtor ctorDef.Body env'

            { ctorDef with Builder = Some(ctorBuilder) }

        let DefineCtors (typeDef : TypeDef, env : Env) =
            let typeDef' =
                { typeDef with
                    Ctors = typeDef.Ctors |> List.map (DefineCtor (typeDef, env))
                }

            (typeDef', env)

        let DefineFields (typeDef : TypeDef, env : Env) =
            let fieldDefs = typeDef.Fields |> List.map (DefineField typeDef)
            let fieldBindings = 
                fieldDefs
                |> List.map (fun fieldDef -> 
                    let fieldBuilder = fieldDef.GetBuilder()
                    (fieldDef.Name, StorageLoc.FieldStorage(StorageLoc.ArgumentStorage(0), fieldBuilder))
                    )

            let env' = new Env(Some(env), Some(fieldBindings))

            (typeDef, env')

        let DefineMembers (typeDef : TypeDef, env : Env) =
            (typeDef, env)
            |> DefineFields
            |> DefineMethods
            |> DefineCtors
            
        let rec DefineNestedType (outerTypeBuilder : TypeBuilder) (typeDef : TypeDef, env : Env) =
            let innerTypeBuilder =
                outerTypeBuilder.DefineNestedType(
                    typeDef.Name,
                    TypeAttributes.Class ||| TypeAttributes.NestedPublic)

            ({ typeDef with Builder = Some(innerTypeBuilder) }, env)
            |> DefineNestedTypes innerTypeBuilder
            |> DefineMembers
            // NOTE -- it is the outer type compiler's responsibility to "create" this type

        and DefineNestedTypes (typeBuilder : TypeBuilder) (typeDef : TypeDef, env : Env) =
            let nestedTypes =
                typeDef.NestedTypes 
                |> List.map (fun inner -> 
                    (inner, env)
                    |> DefineNestedType typeBuilder 
                    |> fst)

            ({ typeDef with NestedTypes = nestedTypes }, env)

        let DefineType (moduleBuilder : ModuleBuilder) (typeDef, env : Env) =
            let typeBuilder =
                moduleBuilder.DefineType(
                    typeDef.Name,
                    TypeAttributes.Class ||| TypeAttributes.Public)

            ({ typeDef with TypeDef.Builder = Some(typeBuilder); }, env)
            |> DefineNestedTypes typeBuilder
            |> DefineMembers

        let CompileType (moduleBuilder : ModuleBuilder) (typeDef : TypeDef, env : Env) =
            let (typeDef', env') = (typeDef, env) |> DefineType moduleBuilder

            // Note the outer type needs to be "created" before the nested types
            let createdType = typeDef'.GetBuilder().CreateType()

            let nestedTypes =
                typeDef'.NestedTypes |> List.map (fun nestedType -> 
                    let nestedTypeBuilder = nestedType.GetBuilder()
                    //let (nestedType', e') = DefineNestedType (typeDef', env)
                    (nestedTypeBuilder.CreateType(), nestedType)
                )
                |> List.unzip

            (typeDef', createdType :: (fst nestedTypes))
//            ({ typeDef' with NestedTypes = snd nestedTypes}, createdType :: (fst nestedTypes))

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

        let env = new Env(None, None)
        let createdTypes = 
            expandedTypes 
            |> List.collect (fun t ->
                let (_, createdTypes) = TypeCompiler.CompileType moduleBuilder (t, env)
                createdTypes
                )
        
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