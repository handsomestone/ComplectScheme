namespace ComplectScheme

module Compiler =
    open System
    open System.Reflection
    open System.Reflection.Emit

    open Util
    open Assembly
    open Symbols
    open Types
    open Expressions
    open Metadata
    open Scope
    open Rewriting

    let getLambdaFuncType (methodDef : MethodDef) =
        let openType = Type.GetType(sprintf "System.Func`%i" (methodDef.Parameters.Length + 1))
        // last type parameter is for return type
        let paramTypes = methodDef.Parameters |> List.map (fun p -> p.Type)
        let returnType = methodDef.ReturnType
        openType.MakeGenericType((List.append paramTypes [ returnType ]) |> List.toArray)

    module PrimitiveOperations =
        let Add (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Add_Ovf)

        let Sub (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Sub_Ovf)

        let CompareEq (ilGen : ILGenerator) =
            ilGen.Emit(OpCodes.Ceq)

    type ExpressionCompiler(ilGen : ILGenerator, typeDef : TypeDef) =
        let emitValue (value : Value) =
            match value with
                | Bool(b) ->
                    if b then 
                        ilGen.Emit(OpCodes.Ldc_I4_1) 
                    else 
                        ilGen.Emit(OpCodes.Ldc_I4_0)
                | Char(c) -> ilGen.Emit(OpCodes.Ldc_I4_S, int c)
                | Int(i) -> ilGen.Emit(OpCodes.Ldc_I4, i)
                | Null -> ilGen.Emit(OpCodes.Ldnull)  // NOTE -- ldnull doesn't seem to work properly wrt stack size

        let emitUnaryOp op =
            match op with
                | UnaryOp.Add1 -> 
                    emitValue (Value.Int(1))
                    PrimitiveOperations.Add ilGen
                | UnaryOp.IsNull ->
                    emitValue (Value.Null)
                    PrimitiveOperations.CompareEq ilGen
                | UnaryOp.IsZero ->
                    emitValue (Value.Int(0))
                    PrimitiveOperations.CompareEq ilGen
                | UnaryOp.Sub1 -> 
                    emitValue (Value.Int(1))
                    PrimitiveOperations.Sub ilGen
                
        let emitBinaryOp op =
            match op with
                | BinaryOp.Add ->
                    PrimitiveOperations.Add ilGen
                | BinaryOp.Sub ->
                    PrimitiveOperations.Sub ilGen

        let emitLocalVariableLoad (li : LocalVariableInfo) =
            ilGen.Emit(OpCodes.Ldloc, li.LocalIndex)
            
        let emitLocalVariableStore (li : LocalVariableInfo) =
            ilGen.Emit(OpCodes.Stloc, li.LocalIndex)

        let emitFieldLoad (fi : FieldInfo) =
            ilGen.Emit(OpCodes.Ldfld, fi)
            
        let emitFieldStore (fi : FieldInfo) =
            ilGen.Emit(OpCodes.Stfld, fi)

        let emitArgumentLoad (pi : ParameterBuilder) =
            ilGen.Emit(OpCodes.Ldarg, pi.Position)

        let emitArgumentStore (pi : ParameterBuilder) =
            ilGen.Emit(OpCodes.Starg, pi.Position)

        let rec emitStorageLoad (stg : StorageLoc) =
            match stg with
                | ArgumentStorage(pi)-> emitArgumentLoad (pi.GetBuilder())
                | FieldStorage(fi) -> 
                    emitFieldLoad (fi.GetBuilder())
                | LocalStorage li -> emitLocalVariableLoad (li.GetBuilder())

        let rec emitStorageStore (stg : StorageLoc) emitValue =
            match stg with
                | ArgumentStorage arg -> 
                    //emitArgumentStore arg
                    failwith "Can't store to argument storage"
                | FieldStorage (fi) -> 
                    emitValue()
                    emitFieldStore (fi.GetBuilder())
                | LocalStorage li ->
                    emitValue()
                    emitLocalVariableStore (li.GetBuilder())
                
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
            
        let storeLocalVariable (binding : Typed<Binding>) : BindingRef =
            let ((id, expr), varType) = binding
            let localBuilder = ilGen.DeclareLocal(varType)
            localBuilder.SetLocalSymInfo(id)

            let localVarDef = {
                Builder = Some(localBuilder);
                Index = localBuilder.LocalIndex;
                Name = id;
                Type = varType;
                }

            let stgLoc = LocalStorage(localVarDef)
            ilGen.Emit(OpCodes.Stloc, localBuilder)
            (id, stgLoc)

        let emitNewObj (ctor : ConstructorInfo) =
            ilGen.Emit(OpCodes.Newobj, ctor)
            
        member this.CompileExpression expr env =
            let rec emitExpr expr env =
                match expr with
                    | Assign(id, e) ->
                        emitVariableAssignment id env (fun () -> emitExpr e env)
                    | BinaryOperation(op, e1, e2) ->
                        emitExpr e1 env
                        emitExpr e2 env
                        emitBinaryOp op
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
                    | Closure(typeId, args, ret) ->
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
                        let invokeType = getLambdaFuncType invokeMethod
                        ilGen.Emit(OpCodes.Ldftn, invokeMethod.GetBuilder())
                        ilGen.Emit(OpCodes.Newobj, invokeType.GetConstructor([| typeof<obj>; typeof<IntPtr> |]))
                    | FunctionCall(e, bindings) ->
                        emitExpr e env
                        bindings |> List.iter (fun binding -> 
                            // TODO -- these should be ordered against the function args, by name?
                            let (id, expr) = binding
                            emitExpr expr env
                            )
                        let invokeMethod = typeof<Func<int,int>>.GetMethod("Invoke")  // TODO -- infer types here
                        ilGen.Emit(OpCodes.Callvirt, invokeMethod)
                    | Immediate(i) -> emitValue i
                    | Lambda(formalParams, capturedParams, e) ->
                        failwithf "Encountered unexpected Lambda expression without closure"
                    | LetBinding(bindings, e) ->
                        let bindingRefs = 
                            bindings 
                            |> List.map (fun binding -> 
                                let (_, expr) = binding
                                let btype = TypeInference.inferType expr
                                emitExpr expr env
                                storeLocalVariable (binding, btype)
                                )
                        emitExpr e (new Env(Some(env), Some(bindingRefs)))
                    | Sequence(exprs) ->
                        exprs |> List.iter (fun e -> emitExpr e env)
                    | UnaryOperation(op, e) ->
                        emitExpr e env
                        emitUnaryOp op
                    | VariableRef(ref, vtype) ->
                        emitVariableRef ref env
            emitExpr expr env
            match TypeInference.inferType expr with
                | t when t = typeof<System.Void> -> ilGen.Emit(OpCodes.Ret)
                | _ -> ilGen.Emit(OpCodes.Ret)

        member this.CompileCtor expr env =
            // Call base class constructor
            ilGen.Emit(OpCodes.Ldarg_0)
            ilGen.Emit(
                OpCodes.Call,
                (typeof<obj>).GetConstructor([||]))

            this.CompileExpression expr env

    (* TODO -- need to figure out exactly the sequence of declaring / defining types and members here should be *) 
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

            let returnType = 
                match methodDef.ReturnType with 
                    | t when t = typeof<System.Void> -> null 
                    | t -> t

            let methodBuilder = 
                typeDef.GetBuilder().DefineMethod(
                    methodDef.Name,
                    methodAttrs,
                    returnType,
                    methodDef.Parameters |> List.map (fun m -> m.Type) |> List.toArray)

            let paramDefs = methodDef.Parameters |> List.mapi (fun i p ->
                let builder = methodBuilder.DefineParameter(i + 1, ParameterAttributes.In, p.Name)
                { p with Builder = Some(builder) })

            let methodDef' = { methodDef with Parameters = paramDefs }

            let exprCompiler = new ExpressionCompiler(methodBuilder.GetILGenerator(), typeDef)

            let argBindings = 
                methodDef'.Parameters
                |> List.map (fun pi -> (pi.Name, StorageLoc.ArgumentStorage(pi)))
            let env' = new Env(Some(env), Some(argBindings))

            exprCompiler.CompileExpression methodDef'.Body env'

            { methodDef' with Builder = Some(methodBuilder) }

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
                    ctorDef.Parameters |> List.map (fun m -> m.Type) |> List.toArray)

            let paramDefs = ctorDef.Parameters |> List.mapi (fun i p ->
                let builder = ctorBuilder.DefineParameter(i + 1, ParameterAttributes.In, p.Name)
                { p with Builder = Some(builder) })

            let ctorDef' = { ctorDef with Parameters = paramDefs }

            let exprCompiler = new ExpressionCompiler(ctorBuilder.GetILGenerator(), typeDef)

            let argBindings = 
                ctorDef'.Parameters
                |> List.map (fun pi -> (pi.Name, StorageLoc.ArgumentStorage(pi)))
            let env' = new Env(Some(env), Some(argBindings))

            exprCompiler.CompileCtor ctorDef'.Body env'

            { ctorDef' with Builder = Some(ctorBuilder) }

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
                |> List.map (fun f -> (f.Name, FieldStorage(f)))

            let env' = new Env(Some(env), Some(fieldBindings))

            ({ typeDef with Fields = fieldDefs }, env')

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

            // NOTE -- the outer type needs to be "created" before the nested types
            let createdType = typeDef'.GetBuilder().CreateType()

            // "create" nested types
            let nestedTypes =
                typeDef'.NestedTypes |> List.map (fun nestedType -> 
                    let nestedTypeBuilder = nestedType.GetBuilder()
                    (nestedTypeBuilder.CreateType(), nestedType)
                )
                |> List.unzip

            (typeDef', createdType :: (fst nestedTypes))

    let compileAssembly asmInfo outFile (typeDefs : TypeDef list) (entryPoint : string) =
        let domain = AppDomain.CurrentDomain
        let asmName = new AssemblyName(asmInfo.AssemblyName)
        let asmBuilder = domain.DefineDynamicAssembly(asmName, AssemblyBuilderAccess.RunAndSave)
        let moduleBuilder = asmBuilder.DefineDynamicModule(asmInfo.ExecutableName, true)

        // Performan lambda-rewriting
        let ctx = { SymGen = new SymbolGenerator() }
        let rewriter = new Rewriter()
        let lambdaRewriter = new LambdaRewriter(ctx)
        let expandedTypes = typeDefs |> List.map (rewriter.RewriteType lambdaRewriter.Rewriter)
        
        // Compile types
        let env = new Env(None, None)
        let createdTypes = 
            expandedTypes 
            |> List.collect (fun t ->
                let (_, createdTypes) = TypeCompiler.CompileType moduleBuilder (t, env)
                createdTypes
                )

        // Find and set the entry point to the assembly
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
                    [("foo", Expr.Immediate(Value.Int(5)))],
                    Expr.Lambda(
                        [("bar", typeof<int>)],
                        [("foo", typeof<int>)],
                        Expr.BinaryOperation(
                            BinaryOp.Add,
                            Expr.VariableRef("bar", typeof<int>),
                            Expr.VariableRef("foo", typeof<int>)))),
                [("bar", Expr.Immediate(Value.Int(2)))])
        expr

    let drive (mainType : Type) args =
        let instance = Activator.CreateInstance(mainType)
        let mainMethod = mainType.GetMethod("Main")
        mainMethod.Invoke(instance, args)

    let build (asmInfo : AssemblyInfo) (mainTypeDef : TypeDef) =
        let createdTypes = compileAssembly asmInfo asmInfo.ExecutableName [ mainTypeDef ] asmInfo.EntryPointName
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
            ReturnType = typeof<int>;
            Parameters = [ { Name = "args"; Type = typeof<string>.MakeArrayType(); Builder = None; Position = 0 } ];
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