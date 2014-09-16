module Expressions
    open System
    open System.Reflection
    open System.Reflection.Emit

    type Value =
        | Int of int
        | Char of char
        | Bool of bool
        | Null

    type Identifier = string
    type TypedIdentifier = Identifier * Type

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