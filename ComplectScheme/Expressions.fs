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
        | Null

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

    let parseExpr parse =
        let rec parseAst ast =
            match ast with
//                | Identifier.List(l) ->
//                    match l with
//                        | [] -> () // TODO
//                        | x :: [] -> ()
//                        | x :: xs -> ()
                | Identifier.Bool(b) -> Expr.Immediate(Value.Bool(b))
                | Identifier.Char(c) -> Expr.Immediate(Value.Char(c))
                | Identifier.Int(i) -> Expr.Immediate(Value.Int(i))
                //| Identifier.Pair() -> ()
        parseAst parse

    let parseString s =
        match (run Parser.parse s) with
            | ParserResult.Success(x, _, _) -> parseExpr x
            | ParserResult.Failure(x, _, _) -> failwith x
