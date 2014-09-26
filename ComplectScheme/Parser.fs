module Parser
    open FParsec

    type Identifier =
        | Symbol of string
        | Char of char
        | Int of int
        | List of Identifier list
        | String of string
        | Bool of bool
        | Pair of Identifier * Identifier

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

    let listOf pElement f =
        let p = (spaces >>. sepBy pElement spaces1 |>> f)
        inBrackets "(" ")" p <|> inBrackets "[" "]" p <|> inBrackets "{" "}" p

    let pairOf pElement f =
        let p = (pipe3 (spaces >>. pElement) (spaces >>. str "." .>> spaces) (pElement .>> spaces) (fun a b c -> a, c)) |>> f
        inBrackets "(" ")" p <|> inBrackets "[" "]" p <|> inBrackets "{" "}" p

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

    let integer =
        pint32 |>> Identifier.Int

    let boolean =
        stringReturn "#t" (Identifier.Bool(true))
        <|> stringReturn "#f" (Identifier.Bool(false))

    let char =
        str "#\\" >>. anyChar |>> Identifier.Char

    let pair =
        pairOf idValue Identifier.Pair

    let quote =
        str "'" >>. idValue |>> (fun x -> Identifier.List([Identifier.Symbol("quote"); x]))

    // Defines the recursive parser to be a choice over the existing parsers
    // NOTE -- symbol should be last as it is the most greedy
    // pair and list are ambiguous, the "attempt" around pair allows us to backtrack if we need to try list as well.
    do idValueRef := choice [ quote; (attempt pair); list; stringLiteral; integer; char; boolean; symbol ]

    // Top-level form should be a list
    let parse = idValue
