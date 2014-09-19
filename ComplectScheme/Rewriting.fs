module Rewriting
    open Expressions
    open Metadata
    open Scope
    open Types

    type RewriterWrapper = (Expr * TypeDef list)

    type RewriterBuilder() =
        member this.Bind(x : RewriterWrapper, f : (Expr -> RewriterWrapper)) : RewriterWrapper =
            let (expr, types) = x
            let (expr', types') = f expr
            (expr', types @ types')

        member this.Return(x) : RewriterWrapper =
            (x, [])

    type Rewriter() =
        member this.RewriteMethod (rewriter : (RewriterWrapper -> RewriterWrapper)) (methodDef : MethodDef) =
            let rw = new RewriterBuilder()
            let rec rewriteExpr expr =
                let wrapper' =
                    match expr with
                        | Immediate(v) -> 
                            rw {
                                return Immediate(v)
                            }
                        | VariableRef(ref, vtype) -> 
                            rw {
                                return VariableRef(ref, vtype)
                            }
                        | UnaryOperation(op, e) -> 
                            rw {
                                let! e' = rewriteExpr e
                                return UnaryOperation(op, e')
                            }
                        | BinaryOperation(op, e1, e2) -> 
                            rw {
                                let! e1' = rewriteExpr e1
                                let! e2' = rewriteExpr e2
                                return BinaryOperation(op, e1', e2')
                            }
                        | LetBinding(b, e) ->
                            rw {
                                let! e' = rewriteExpr e
                                return LetBinding(b, e')
                            }
                        | Conditional(e1, e2, e3) -> 
                            rw {
                                let! e1' = rewriteExpr e1
                                let! e2' = rewriteExpr e2
                                let! e3' = rewriteExpr e3
                                return Conditional(e1', e2', e3')
                            }
                        | FunctionCall (e, b) -> 
                            rw {
                                let! e' = rewriteExpr e
                                return FunctionCall(e', b)
                            }
                        | Lambda(p1, p2, e) -> 
                            rw {
                                let! e' = rewriteExpr e
                                return Lambda(p1, p2, e')
                            }
                        | Closure(id, p, ret) ->
                            rw {
                                return Closure(id, p, ret)
                            }
                        | Assign(id, e) -> 
                            rw {
                                let! e' = rewriteExpr e
                                return Assign(id, e')
                            }
                        | Sequence(es) -> 
                            rw { 
                                let wrappers = es |> List.map rewriteExpr
                                let es' = wrappers |> List.map fst
                                return Sequence(es')
                            }
                rewriter wrapper'
            let (rewritten, types') = rewriteExpr methodDef.Body

            ({ methodDef with Body = rewritten }, types')

        member this.RewriteType rewriter (typeDef : TypeDef) =
            let (methods', types') = 
                List.foldBack (fun m (ms, ts) -> 
                    let (rewritten, newTypes) = this.RewriteMethod rewriter m
                    (rewritten :: ms, newTypes @ ts)
                    ) typeDef.Functions ([], [])
            { typeDef with Functions = methods'; NestedTypes = typeDef.NestedTypes @ types' }
        
    type LambdaRewriter(ctx : CompilerContext) =
        let createLambdaType (formalParams : Typed<Identifier> list) (capturedParams : Typed<Identifier> list) expr =
            let typedIds2ParameterDefs idList =
                idList 
                |> List.mapi (fun i (id, ptype) -> 
                    { 
                        Builder = None;
                        Type = ptype;
                        Name = id;
                        Position = i;
                    })

            let formalParamDefs = typedIds2ParameterDefs formalParams

            let returnType = TypeInference.inferType expr

            let invokeMethod : MethodDef = {
                Name = "Invoke";
                Builder = None;
                Body = expr;  // TODO -- rewrite storage locs?
                ReturnType = returnType;  // ???
                Parameters = formalParamDefs;
                IsStatic = false;
            }

            let capturedParamDefs = 
                capturedParams 
                |> typedIds2ParameterDefs
                |> List.map (fun p -> { p with Name = "_" + p.Name })

            let capturedFieldDefs =
                capturedParams
                |> List.map (fun (id, ptype) -> 
                    {
                        FieldDef.Builder = None;
                        FieldDef.Name = id;
                        FieldDef.Type = ptype;
                    })

            let ctorExpr = 
                Expr.Sequence(
                    capturedParams |> List.map (fun (id, t) ->
                        Expr.Assign(id, Expr.VariableRef("_" + id, t))
                        ))

            let ctor : CtorDef = {
                Builder = None;
                Body = ctorExpr;
                Parameters = capturedParamDefs
            }

            let lambdaType : TypeDef = {
                Name = (sprintf "lambda%i" (ctx.SymGen.GetNewSymbol()));  // TODO -- can we get a fully qualified type here?
                Builder = None
                Functions = [ invokeMethod ];
                Ctors = [ ctor ];
                NestedTypes = [];
                IsNested = true;
                Fields = capturedFieldDefs;
                }

            (lambdaType, returnType)

        let defaultTypes (ids : Identifier list) =
            ids |> List.map (fun id -> (id, typeof<int>))

        member this.Rewriter (wrapper : RewriterWrapper) =
            let (expr, types) = wrapper
            match expr with
                | Lambda(formalParams, capturedParams, e) ->
                    let (closure, returnType) = createLambdaType formalParams capturedParams e
                    // TODO -- need to replace the Lambda expr with a new instance of lambda
                    (Expr.Closure(closure.Name, capturedParams, returnType), closure :: types)
                | other -> (other, types)