module Parser
    open FParsec

    type Identifier = string
    type Keyword = string

    type Constant =
        | Character of char
        | Number of int
        | String of string
        | Boolean of bool

    type Datum =
        | Boolean of bool
        | Number of int
        | Character of char
        | String of string
        | Symbol of string
        | List of List
        | Pair of Datum * Datum
    and List = Datum list

    type Definition =
        | VariableDef of VariableDef
    and Expression =
        | Constant of Constant
        | Variable of Identifier
        | Quote of Datum
        | IfThen of Expression * Expression
        | IfThenElse of Expression * Expression * Expression
        | Application of Expression * Expression list
        | LetSyntax of SyntaxBinding list * Expression list
        | LetRecSyntax of SyntaxBinding list * Expression list
    and SyntaxBinding = Keyword * Expression
    and VariableDef =
        | VariableExpr of Identifier * Expression

    // Characters and string functions
    let str s = pstring s
    
    let reservedChars = Set.ofList [ '('; ')'; '['; ']'; '{'; '}'; '\''; '.' ]
    let isPunc c = System.Char.IsPunctuation(c)
    let isSymbol c = System.Char.IsSymbol(c)
    let isReserved c = reservedChars |> Set.contains c
    let validStringContents c = c <> '"'

    let pSymbolEscape = 
        str "\\" >>. anyChar 
        // <|> str "|" >>. manySatisfy (isNoneOf [ '|' ])

    let pInitial = 
        letter <|>
        digit <|>
        pSymbolEscape <|>
        satisfy (isAnyOf [ '.'; '+'; '-'; '!'; '$'; '%'; '&'; '*'; '/'; ':'; '<'; '='; '>'; '?'; '~'; '_'; '^'; '@' ])
    let pSubsequent =
        pInitial <|> pchar '#'

    let pIdentifier =
        many1Chars2 pInitial pSubsequent

    let pTuple2WithSpaces p1 p2 =
        (p1 .>> spaces1) .>>. p2

    let pTuple3WithSpaces p1 p2 p3 =
        tuple3 (p1 .>> spaces1) (p2 .>> spaces1) p3

    let inBrackets bopen bclose p =
        between (str bopen) (str bclose) p

    let inAnyBrackets p =
        inBrackets "(" ")" p <|> inBrackets "[" "]" p <|> inBrackets "{" "}" p

    let listOf pElement =
        let p = (spaces >>. sepEndBy pElement spaces1)
        inAnyBrackets p

    let listForm pFirst pRest =
        let p = (spaces >>. (pTuple2WithSpaces pFirst pRest) .>> spaces)
        inAnyBrackets p

    let listFormIgnore1 pFirst pRest =
        let p = (spaces >>. (pTuple2WithSpaces pFirst pRest |>> snd) .>> spaces)
        inAnyBrackets p

    let pairOf pElement =
        let p = spaces >>. (pipe3 pElement (spaces >>. str "." .>> spaces) pElement (fun a b c -> a, c)) .>> spaces
        inAnyBrackets p

    // Forward declaration of parser, needed for recursive parser
    let pDatum, pDatumRef = createParserForwardedToRef<Datum, unit>()
    let pExpr, pExprRef = createParserForwardedToRef<Expression, unit>()
    let pDef, pDefRef = createParserForwardedToRef<Definition, unit>()

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

    let pPair =
        pairOf pDatum

    let pQuote =
        skipString "'" .>> spaces >>. pDatum
        <|> listFormIgnore1 (skipString "quote") pDatum

    let pVariable =
        pIdentifier

    let pIfThen =
        listFormIgnore1 (str "if") (pTuple2WithSpaces pExpr pExpr)

    let pIfThenElse =
        listFormIgnore1 (str "if") (pTuple3WithSpaces pExpr pExpr pExpr)

    let pApplication =
        listForm pExpr (sepEndBy pExpr spaces1)

    let pKeyword =
        pIdentifier

    let pSyntaxBinding =
        listForm pKeyword pExpr

    let pLetSyntax =
        listFormIgnore1 (str "let-syntax") (pTuple2WithSpaces (listOf pSyntaxBinding) (sepEndBy pExpr spaces1))

    let pLetRecSyntax =
        listFormIgnore1 (str "letrec-syntax") (pTuple2WithSpaces (listOf pSyntaxBinding) (sepEndBy pExpr spaces1))

    let pVariableExprDef =
        pTuple2WithSpaces pVariable pExpr

    let pVariableDef =
        let defBody = 
            choice [
                pVariableExprDef |>> VariableExpr
                ]
        listFormIgnore1 (str "define") defBody

    // TODO -- nested comments
//    let lineComment =
//        str ";" >>. restOfLine false |>> Datum.Comment

    do pDatumRef :=
        choice [ 
            attempt (pPair |>> Datum.Pair);
            pList |>> Datum.List;
            pStringLiteral |>> Datum.String;
            pInteger |>> Datum.Number;
            pChar |>> Datum.Character;
            pBoolean |>> Datum.Boolean;
            pSymbol |>> Datum.Symbol; 
            ]

    let constant =
        choice [
            pStringLiteral |>> Constant.String;
            pInteger |>> Constant.Number;
            pChar |>> Constant.Character;
            pBoolean |>> Constant.Boolean; 
            ]

    do pExprRef := 
        choice [ 
            constant |>> Expression.Constant;
            pVariable |>> Expression.Variable;
            attempt (pIfThen |>> Expression.IfThen);
            attempt (pIfThenElse |>> Expression.IfThenElse);
            attempt (pQuote |>> Expression.Quote);
            attempt (pLetSyntax |>> Expression.LetSyntax)
            attempt (pLetRecSyntax |>> Expression.LetRecSyntax)
            attempt (pApplication |>> Expression.Application);
            ]

    do pDefRef :=
        choice [
            pVariableDef |>> Definition.VariableDef;
            ]

    let parseDefinition = pDef
    let parseExpr = pExpr
    let parseDatum = pDatum

    // Parse multiple "statements"
    let parseAll = spaces >>. many (pExpr .>> spaces)
