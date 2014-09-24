module Expressions
    open System
    open System.Reflection
    open System.Reflection.Emit

    type Value =
        | Bool of bool
        | Char of char
        | Int of int
        | List of Value list
        | Null
        | Pair of Value * Value

    type Identifier = string

    type Typed<'a> = 'a * Type

    type UnaryOp =
        | Add1
        | IsNull
        | IsZero
        | Sub1

    type BinaryOp =
        | Add
        | Sub

    type Expr =
        | Assign of Identifier * Expr
        | BinaryOperation of BinaryOp * Expr * Expr
        | Closure of Identifier * Typed<Identifier> list * Type
        | Conditional of Expr * Expr * Expr
        | FunctionCall of Expr * Binding list
        | Immediate of Value
        | Lambda of Typed<Identifier> list * Typed<Identifier> list * Expr
        | LetBinding of Binding list * Expr
        | Sequence of Expr list
        | UnaryOperation of UnaryOp * Expr
        | VariableRef of Identifier * Type
    and Binding = Identifier * Expr

    let pairsFromList l =
        List.foldBack (fun t s -> Pair(t, s)) l (Value.Null)
