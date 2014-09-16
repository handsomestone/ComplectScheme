module Metadata
    open System
    open System.Reflection
    open System.Reflection.Emit

    open Assembly
    open Symbols
    open Expressions

    type CompilerInfo = {
        Domain : AppDomain;
        AsmName : AssemblyName;
        AsmBuilder : AssemblyBuilder;
        ModuleBuilder : ModuleBuilder;
        }

    type CompilerContext = {
        SymGen : SymbolGenerator;
        }

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
