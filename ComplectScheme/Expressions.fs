module Expressions
    open System
    open System.Reflection
    open System.Reflection.Emit

    open FParsec
    open Parser

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

    let parseExpr parse =
        let rec parseValue ast =
            match ast with
                | Identifier.Bool(b) -> Value.Bool(b)
                | Identifier.Char(c) -> Value.Char(c)
                | Identifier.Int(i) -> Value.Int(i)
                | Identifier.List(l) -> Value.List(l |> List.map parseValue)
                | Identifier.Pair(a, b) -> Value.Pair((parseValue a), (parseValue b))
                | Identifier.String(s) -> failwith "string unsupported"
                | Identifier.Symbol(s) -> failwith "symbol unsupported"
        Expr.Immediate(parseValue parse)

    let parseString s =
        match (run Parser.parse s) with
            | ParserResult.Success(x, _, _) -> parseExpr x
            | ParserResult.Failure(x, _, _) -> failwith x
