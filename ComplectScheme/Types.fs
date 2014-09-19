module Types
    open System

    open Expressions
    open Scope

    module TypeInference =
        let inferValueType v =
            match v with
                | Bool(b) -> typeof<bool>
                | Char(c) -> typeof<char>
                | Int(i) -> typeof<int>
                | Null -> typeof<System.Void>

        let inferType (expr : Expr) : Type =
            let rec inferTypes exprs =
                match exprs |> List.map inferTypeRec with
                    | t :: rest when rest |> List.forall ((=) t) -> t
                    | _ -> failwithf "Type mismatch for expression"
            and inferTypeRec (expr : Expr) : Type =
                match expr with
                    | Assign(id, e) -> inferTypeRec e
                    | BinaryOperation(op, e1, e2) -> inferTypes [e1; e2]
                    | Conditional(test, e1, e2) -> inferTypes [e1; e2]
                    | Closure(typeId, args, ret) -> ret
                    | FunctionCall(e, bindings) -> inferTypeRec e  // TODO -- trace?
                    | Immediate(i) -> inferValueType i
                    | Lambda(formalParams, capturedParams, e) -> inferTypeRec e
                    | LetBinding(bindings, e) -> inferTypeRec e
                    | Sequence(exprs) -> inferTypeRec (exprs |> Seq.last)  // TODO -- not exactly sure what this should be
                    | UnaryOperation(op, e) -> inferTypeRec e
                    | VariableRef(id, vtype) -> vtype
//                        match env.FindIdentifier id with
//                            | None -> failwithf "Unable to find variable reference %s" id
//                            | Some(stg) -> 
//                                match stg with
//                                    | ArgumentStorage(arg) -> arg.Type
//                                    | FieldStorage(fld) -> fld.Type
//                                    | LocalStorage(loc) -> loc.Type
            inferTypeRec expr