module Compiler =
    open System
    open System.Reflection
    open System.Reflection.Emit

    type AssemblyInfo = {
        AssemblyName : string;
        ExecutableName : string;
        MainClassName : string;
        EntryPointName : string }

    let compile asmInfo outFile generateIL =
        let domain = AppDomain.CurrentDomain  //AppDomain.CreateDomain("Sandbox")
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
        
        // TODO -- pass string args to Console.WriteLine
        ilGen.Emit(OpCodes.Ldarg_0)
        ilGen.Emit(OpCodes.Ldelem, 0)
        
        ilGen.Emit(OpCodes.Ldc_I4, 73)
        ilGen.Emit(OpCodes.Ret)

    [<EntryPoint>]
    let main argv = 
        let asmInfo = { AssemblyName = "complect"; EntryPointName = "Main"; MainClassName = "MainClass"; ExecutableName = "program.exe" }
        let mainType = compile asmInfo asmInfo.ExecutableName generateMain

        let instance = Activator.CreateInstance(mainType)
        let mainMethod = mainType.GetMethod("Main")
        let ret = mainMethod.Invoke(instance, [| [|"hello"; "world"|] |])
        //let ret = mainMethod.Invoke(instance, [| |])

        printfn "%A" ret
        Console.ReadLine() |> ignore
        0