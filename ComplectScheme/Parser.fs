module Parser
    open FParsec

    type Identifier = string

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

    type Expression =
        | Constant of Constant
        | Variable of Identifier
        | Quote of Datum
        //| Lamda of Formals * Body
    //and Formals = Variable list
    //and Body = Definition * Expression

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

    let inBrackets bopen bclose p =
            between (str bopen) (str bclose) p

    let inAnyBrackets p =
        inBrackets "(" ")" p <|> inBrackets "[" "]" p <|> inBrackets "{" "}" p

    let listOf pElement =
        let p = (spaces >>. sepEndBy pElement spaces1)
        inAnyBrackets p

    let listForm pFirst pRest =
        let p = (spaces >>. pFirst >>. spaces1 >>. pRest)
        inAnyBrackets p

    let pairOf pElement =
        let p = (pipe3 (spaces >>. pElement) (spaces >>. str "." .>> spaces) (pElement .>> spaces) (fun a b c -> a, c))
        inAnyBrackets p

    // Forward declaration of parser, needed for recursive parser
    let pDatum, pDatumRef = createParserForwardedToRef<Datum, unit>()

    let pExpr, pExprRef = createParserForwardedToRef<Expression, unit>()

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
        str "'" .>> spaces >>. pDatum
        <|> listForm (str "quote") pDatum

    let pVariable =
        pIdentifier

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
            pSymbol |>> Datum.Symbol ]

    let constant =
        choice [
            pStringLiteral |>> Constant.String;
            pInteger |>> Constant.Number;
            pChar |>> Constant.Character;
            pBoolean |>> Constant.Boolean ]

    do pExprRef := choice [ 
        constant |>> Expression.Constant;
        pVariable |>> Expression.Variable;
        pQuote |>> Expression.Quote;
        ]

    // Top-level form should be a list
    let parseExpr = pExpr

    let parseDatum = pDatum

    // Parse multiple "statements"
    let parseAll = spaces >>. many (pExpr .>> spaces)
