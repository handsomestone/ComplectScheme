module Scope
    open Expressions
    open Metadata

    type StorageLoc =
        | ArgumentStorage of ParameterDef
        | FieldStorage of StorageLoc * FieldDef
        | LocalStorage of LocalVariableDef

    type BindingRef = Identifier * StorageLoc

    type Env(env : Env option, bindings : BindingRef list option) =
        member private this.map = new Map<Identifier, StorageLoc>(match bindings with Some(b) -> (Seq.ofList b) | None -> Seq.empty)

        member private this.merge (other : BindingRef list) =
            match bindings with
                | Some(b) -> other @ b
                | None -> other

        member this.FindIdentifier id =
            match (this.map |> Map.tryFind id), env with
                | Some stg, _-> Some stg
                | None, Some e -> e.FindIdentifier id
                | None, None -> None

        member this.MergeWith (other : Env) =
            let combined =
                match bindings with
                    | Some(b) -> other.merge b
                    | None -> other.merge []
            new Env(env, Some(combined))
