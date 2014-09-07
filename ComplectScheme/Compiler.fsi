namespace ComplectScheme

open System.Reflection.Emit

module Compiler =
    type AssemblyInfo
//    type AssemblyInfo = {
//        AssemblyName : string;
//        ExecutableName : string;
//        MainClassName : string;
//        EntryPointName : string
//        }

    val compile : AssemblyInfo -> string -> (ILGenerator -> unit) -> System.Type
