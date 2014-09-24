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
        //| String of string
        //| Symbol of string ???

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

    let parseValue v =
        match v with
            | Identifier.Bool(b) -> Value.Bool(b)
            | Identifier.Char(c) -> Value.Char(c)
            | Identifier.Int(i) -> Value.Int(i)
            | _ -> failwith "not a value"

    let quote l =
        Expr.Immediate(Value.List(l |> List.map parseValue))

    let tryQuote =
        function
            | Identifier.List(l) :: [] -> quote l
            | _ -> failwithf "quote: wrong number of parts"

    let parseApplication a args =
        match a with
            | "quote" -> tryQuote args
            | _ -> failwithf "%s: undefined" a

    let parseExpr parse =
        let rec parseList l =
            match l with
                | Identifier.Symbol(a) :: args -> parseApplication a args
        and parseExpr' ast =
            match ast with
                | Identifier.Bool(b) -> Expr.Immediate(parseValue ast)
                | Identifier.Char(c) -> Expr.Immediate(parseValue ast)
                | Identifier.Int(i) -> Expr.Immediate(parseValue ast)
                | Identifier.List(l) ->
                    parseList l
                //| Identifier.Pair(a, b) -> Expr.Immediate(Value.Pair((parseExpr' a), (parseExpr' b)))
                //| Identifier.String(s) -> Value.String(s)
                //| Identifier.Symbol(s) -> Value.Symbol(s)
        parseExpr' parse

    let parseString s =
        match (run Parser.parse s) with
            | ParserResult.Success(x, _, _) -> parseExpr x
            | ParserResult.Failure(x, _, _) -> failwith x
