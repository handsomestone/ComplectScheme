module Parser
    open FParsec

    type Identifier = Identifier of string
    type Variable = Variable of Identifier

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
        | Variable of Variable
        | Quote of Datum
        //| Lamda of Formals * Body
    //and Formals = Variable list
    //and Body = Definition * Expression

    // Characters and string functions
    let reservedChars = Set.ofList [ '('; ')'; '['; ']'; '{'; '}'; '\''; '.' ]
    let isPunc c = System.Char.IsPunctuation(c)
    let isSymbol c = System.Char.IsSymbol(c)
    let isReserved c = reservedChars |> Set.contains c
    let validStringContents c = c <> '"'
    let validChars c = (isLetter c || isDigit c || isPunc c || isSymbol c) && not (isReserved c)

    // Convenience functions
    let str s = pstring s
    
    let inBrackets bopen bclose p =
            between (str bopen) (str bclose) p

    let listOf pElement =
        let p = (spaces >>. sepBy pElement spaces1)
        inBrackets "(" ")" p <|> inBrackets "[" "]" p <|> inBrackets "{" "}" p

    let pairOf pElement =
        let p = (pipe3 (spaces >>. pElement) (spaces >>. str "." .>> spaces) (pElement .>> spaces) (fun a b c -> a, c))
        inBrackets "(" ")" p <|> inBrackets "[" "]" p <|> inBrackets "{" "}" p

    // Forward declaration of parser, needed for recursive parser
    let pDatum, pDatumRef = createParserForwardedToRef<Datum, unit>()

    let pExpr, pExprRef = createParserForwardedToRef<Expression, unit>()

    // ( ... )
    let list =
        listOf pDatum

    // non-quoted string
    let symbol =
        (many1Satisfy validChars)

    // quoted string
    let stringLiteral =
        // TODO -- implement escape characters
        between (str "\"") (str "\"") (manySatisfy validStringContents)

    let integer =
        pint32

    let boolean =
        stringReturn "#t" true
        <|> stringReturn "#f" false

    let char =
        str "#\\" >>. anyChar

    let pair =
        pairOf pDatum

    let quote =
        str "'" >>. pDatum

    // TODO -- nested comments
//    let lineComment =
//        str ";" >>. restOfLine false |>> Datum.Comment

    do pDatumRef :=
        choice [ 
            attempt (pair |>> Datum.Pair);  // ( <val> . <val> )
            list |>> Datum.List;  // ( <val> ... )
            stringLiteral |>> Datum.String;  // "<char>..."
            integer |>> Datum.Number;  // <int>
            char |>> Datum.Character;  // #\<char>
            boolean |>> Datum.Boolean;  // #t or #f
            symbol |>> Datum.Symbol ]  // <chars>]

    let constant =
        choice [
            stringLiteral |>> Constant.String;  // "<char>..."
            integer |>> Constant.Number;  // <int>
            char |>> Constant.Character;  // #\<char>
            boolean |>> Constant.Boolean ]  // #t or #f

    // Defines the recursive parser to be a choice over the existing parsers
    // NOTE -- symbol should be last as it is the most greedy
    // pair and list are ambiguous, the "attempt" around pair allows us to backtrack if we need to try list as well.
    do pExprRef := choice [ 
        constant |>> Expression.Constant;
        //lineComment;  // ; comment
        quote |>> Expression.Quote;  // '<val>
        //(attempt pair);  // ( <val> . <val> )
        ]

    // Top-level form should be a list
    let parseExpr = pExpr

    let parseDatum = pDatum

    // Parse multiple "statements"
    let parseAll = spaces >>. many (pExpr .>> spaces)
