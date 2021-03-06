﻿module Types
    open System

    open Expressions
    open Scope

    module TypeInference =
        let rec inferValueType v =
            match v with
                | Bool(b) -> typeof<bool>
                | Char(c) -> typeof<char>
                | Int(i) -> typeof<int>
                | Null -> typeof<obj>
                | Pair(a, b) -> 
                    let types = [| (inferValueType a); (inferValueType b) |]
                    Type.GetType("System.Tuple`2").MakeGenericType(types)
                | List(l) ->
                    let rootPair = pairsFromList l
                    inferValueType rootPair

        let inferBinaryOpType op =
            typeof<int>  // assume all built-in binary ops are int valued for now

        let inferUnaryOpType op =
            match op with
                | UnaryOp.Add1 -> typeof<int>
                | UnaryOp.IsNull -> typeof<bool>
                | UnaryOp.IsZero -> typeof<bool>
                | UnaryOp.Sub1 -> typeof<int>

        let inferType (expr : Expr) : Type =
            let rec inferTypes exprs =
                match exprs |> List.map inferTypeRec with
                    | t :: rest when rest |> List.forall ((=) t) -> t
                    | _ -> failwithf "Type mismatch for expression"
            and inferTypeRec (expr : Expr) : Type =
                match expr with
                    | Assign(id, e) -> typeof<System.Void>
                    | BinaryOperation(op, e1, e2) -> inferBinaryOpType op
                    | Conditional(test, e1, e2) -> inferTypes [e1; e2]
                    | Closure(typeId, args, ret) -> ret  // NOTE -- may want to return something else here, e.g. Closure<ret>
                    | FunctionCall(e, bindings) -> inferTypeRec e  // TODO -- trace?
                    | Immediate(i) -> inferValueType i
                    | Lambda(formalParams, capturedParams, e) -> inferTypeRec e
                    | LetBinding(bindings, e) -> inferTypeRec e
                    | Sequence(exprs) -> inferTypeRec (exprs |> Seq.last)  // TODO -- not exactly sure what this should be
                    | UnaryOperation(op, e) -> inferUnaryOpType op
                    | VariableRef(id, vtype) -> vtype
            inferTypeRec expr
