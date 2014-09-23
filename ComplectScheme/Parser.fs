module Parser
    open FParsec

    type Identifier =
        | Symbol of string
        | Char of char
        | Int of int
        | List of Identifier list
        | String of string

    // Characters and string functions
    let reservedChars = Set.ofList [ '('; ')'; '\'' ]
    let isPunc c = System.Char.IsPunctuation(c)
    let isSymbol c = System.Char.IsSymbol(c)
    let isReserved c = reservedChars |> Set.contains c
    let validStringContents c = c <> '"'
    let validChars c = (isLetter c || isDigit c || isPunc c || isSymbol c) && not (isReserved c)

    // Convenience functions
    let str s = pstring s    
    let listOf pElement f =
        between (str "(") (str ")") (spaces >>. sepBy pElement spaces1 |>> f)

    // Forward declaration of parser, needed for recursive parser
    let idValue, idValueRef = createParserForwardedToRef<Identifier, unit>()

    // ( ... )
    let list =
        listOf idValue Identifier.List

    // non-quoted string
    let symbol : Parser<Identifier, unit> =
        (many1Satisfy validChars) |>> Identifier.Symbol

    // quoted string
    let stringLiteral =
        // TODO -- implement escape characters
        between (str "\"") (str "\"") (manySatisfy validStringContents) |>> Identifier.String

    // Defines the recursive parser to be a choice over the existing parsers
    do idValueRef := choice [ stringLiteral; list; symbol ]

    // Top-level form should be a list
    let parse = list
