﻿module Parser
    open FParsec

    type Identifier = string
    type Keyword = string

    type Datum =
        | Boolean of bool
        | Number of int
        | Character of char
        | String of string
        | Symbol of string
        | List of List
    and List = Datum list

    type Program =
        | Forms of Form list
    and Form =
        | Definition of Definition
        | Expression of Expression
    and Definition =
        | VariableExpr of Identifier * Expression
        | VariableLambda of Identifier * Identifier list * Body
        | DefSyntax of Keyword * TransformerSpec
    and Expression =
        | Literal of Datum
        | Variable of Variable
        | Conditional of Expression * Expression * Expression option
        | Application of Expression * Expression list
        | Lambda of Variable list * Body
        | LetSyntax of SyntaxBinding list * Body
        | LetRecSyntax of SyntaxBinding list * Body
        | Let of Binding list * Body
        | LetRec of Binding list * Body
    and Variable = Identifier
    and Formals = Variable list
    and Binding = Identifier * Expression
    and SyntaxBinding = Keyword * TransformerSpec
    and Body = Definition list * Expression list
    and TransformerSpec = Identifier list * SyntaxRule list
    and SyntaxRule = Pattern * Template
    and Pattern = 
        | Identifier of Identifier
        | Literal of Datum
        // TODO -- pattern lists
    and Template = 
        | Identifier of Identifier
        | Literal of Datum
        // TODO -- template lists

    // Characters and string functions
    let str s = pstring s
    let validStringContents c = c <> '"'
    let expressionKeywords = [ "quote"; "lambda"; "if"; "set!"; "begin"; "cond"; "and"; "or"; "case"; "let"; "let*"; "letrec"; "do"; "delay"; "quasiquote" ]
    let syntacticKeywords = expressionKeywords @ [ "else"; "=>"; "define"; "unquote"; "unquote-splicing" ]

    let pSymbolEscape = 
        str "\\" >>. anyChar

    let except (p : Parser<'a,_>) (predicate : 'a -> bool) msg =
        p >>= (fun x -> 
            match predicate x with
                | false -> preturn x
                | true -> fail msg)

    let pInitial = 
        letter <|>
        digit <|>
        pSymbolEscape <|>
        satisfy (isAnyOf [ '!'; '$'; '%'; '&'; '*'; '/'; ':'; '<'; '='; '>'; '?'; '^'; '_'; '~' ])
    let pSubsequent =
        pInitial <|> satisfy (isAnyOf [ '+'; '-'; '.'; '@' ])

    let pIdentifier =
        many1Chars2 pInitial pSubsequent
        <|> (str "+") <|> (str "-") <|> (str "...")

    let pTuple2WithSpaces p1 p2 =
        (p1 .>> spaces) .>>. p2

    let pTuple3WithSpaces p1 p2 p3 =
        tuple3 (p1 .>> spaces1) (p2 .>> spaces1) p3

    let inBrackets bopen bclose p =
        between (str bopen) (str bclose) p

    let inAnyBrackets p =
        inBrackets "(" ")" p <|> inBrackets "[" "]" p <|> inBrackets "{" "}" p

    let listForm pFirst pRest =
        let p = (spaces >>. (pTuple2WithSpaces pFirst pRest) .>> spaces)
        inAnyBrackets p

    let listFormIgnore1 pFirst pRest =
        let p = (spaces >>. (pTuple2WithSpaces pFirst pRest |>> snd) .>> spaces)
        inAnyBrackets p

    let listOf pElement =
        attempt (listForm (sepEndBy1 pElement spaces1) (str "." >>. spaces1 >>. pElement) |>> (fun (a, b) -> a @ [ b ]))
        <|> inAnyBrackets (spaces >>. sepEndBy pElement spaces1)

    // Forward declaration of parser, needed for recursive parser
    let pDatum, pDatumRef = createParserForwardedToRef<Datum, unit>()
    let pExpr, pExprRef = createParserForwardedToRef<Expression, unit>()
    let pDef, pDefRef = createParserForwardedToRef<Definition, unit>()

    let pBody =
        (sepEndBy pDef spaces1) .>>. (sepEndBy pExpr spaces1)

    // ( ... )
    let pList =
        listOf pDatum

    // non-quoted string
    let pSymbol =
        pIdentifier

    // quoted string
    let pStringLiteral =
        // TODO -- implement escape characters
        between (str "\"") (str "\"") (manySatisfy validStringContents)

    let pInteger =
        pint32

    let pBoolean =
        stringReturn "#t" true
        <|> stringReturn "#f" false

    let pChar =
        str "#\\" >>. anyChar

    let pQuote =
        skipString "'" .>> spaces >>. pDatum
        <|> listFormIgnore1 (skipString "quote") pDatum

    let pLiteral =
        choice [
            pStringLiteral |>> Datum.String;
            pInteger |>> Datum.Number;
            pChar |>> Datum.Character;
            pBoolean |>> Datum.Boolean;
            attempt pQuote
            ]

    let pVariable = 
        except pIdentifier (fun p -> 
            List.tryFind ((=) p) syntacticKeywords 
            |> function None -> false | _ -> true)
            "reserved identifier"

    let pIfThen =
        listFormIgnore1 (str "if") (pTuple2WithSpaces pExpr pExpr)

    let pIfThenElse =
        listFormIgnore1 (str "if") (pTuple3WithSpaces pExpr pExpr pExpr)

    let pApplication =
        listForm pExpr (sepEndBy pExpr spaces1)

    let pKeyword =
        pIdentifier

    let pPattern =
        choice [
            pIdentifier |>> Pattern.Identifier;
            pLiteral |>> Pattern.Literal;
            ]

    let pTemplate =
        choice [
            pIdentifier |>> Template.Identifier;
            pLiteral |>> Template.Literal;
            ]

    let pSyntaxRule : Parser<SyntaxRule, _> =
        listForm pPattern pTemplate

    let pTransformerSpec : Parser<TransformerSpec, _> =
        listFormIgnore1 (str "syntax-rules") ((listOf pIdentifier) .>>. (listOf pSyntaxRule))

    let pSyntaxBinding : Parser<SyntaxBinding, _> =
        listForm pKeyword pTransformerSpec

    let pBinding : Parser<Binding, _> =
        listForm pVariable pExpr

    let pLetSyntax =
        listFormIgnore1 (str "let-syntax") (pTuple2WithSpaces (listOf pSyntaxBinding) pBody)

    let pLetRecSyntax =
        listFormIgnore1 (str "letrec-syntax") (pTuple2WithSpaces (listOf pSyntaxBinding) pBody)

    let pLet : Parser<Binding list * Body, _> =
        listFormIgnore1 (str "let") (pTuple2WithSpaces (listOf pBinding) pBody)

    let pLetRec : Parser<Binding list * Body, _> =
        listFormIgnore1 (str "letrec") (pTuple2WithSpaces (listOf pBinding) pBody)

    let pVariableExprDef =
        pTuple2WithSpaces pVariable pExpr

    let pForm =
        (pDef |>> Form.Definition) <|> (pExpr |>> Form.Expression)

    let pVariableLambdaDef =
        (listForm pVariable (sepEndBy pVariable spaces1) .>> spaces1) .>>. pBody |>> (fun ((a, b), c) -> a, b, c)

    let pProgram =
        spaces >>. (sepEndBy pForm spaces1)

    // TODO -- nested comments
//    let lineComment =
//        str ";" >>. restOfLine false |>> Datum.Comment

    do pDatumRef :=
        choice [
            pList |>> Datum.List;
            pStringLiteral |>> Datum.String;
            pInteger |>> Datum.Number;
            pChar |>> Datum.Character;
            pBoolean |>> Datum.Boolean;
            pSymbol |>> Datum.Symbol; 
            ]

    do pExprRef := 
        choice [ 
            pLiteral |>> Expression.Literal;
            pVariable |>> Expression.Variable;
            attempt (pIfThen |>> (fun (x, y) -> Expression.Conditional(x, y, None)));
            attempt (pIfThenElse |>> (fun (x, y, z) -> Expression.Conditional(x, y, Some z)));
            //attempt (pLetSyntax |>> Expression.LetSyntax)
            //attempt (pLetRecSyntax |>> Expression.LetRecSyntax)
            attempt (pLet |>> Expression.Let)
            attempt (pLetRec |>> Expression.LetRec)
            attempt (pApplication |>> Expression.Application);
            ]

    do pDefRef :=
        let defBody = 
            attempt (pVariableLambdaDef |>> VariableLambda)
            <|> (pVariableExprDef |>> VariableExpr)
        listFormIgnore1 (str "define") defBody

    let parseDefinition = pDef
    let parseExpr = pExpr
    let parseDatum = pDatum
    let parseForm = pForm
    let parseProgram = pProgram
