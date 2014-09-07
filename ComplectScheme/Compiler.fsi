namespace ComplectScheme

open System.Reflection.Emit

module Compiler =
    type AssemblyInfo

    val compile : AssemblyInfo -> string -> (ILGenerator -> unit) -> System.Type

    type Value
    type Op
    type Expr

    type ILEmitter =
        new : ILGenerator -> ILEmitter
        member EmitExpr : Expr -> unit
