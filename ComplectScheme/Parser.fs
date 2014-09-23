module Parser
    open FParsec

    type UserState = unit  // TODO -- may want to pass some useful state at some point, but required for value restriction
    type Parser<'t> = Parser<'t, UserState>

    let reservedChars = Set.ofList [ '('; ')'; '\'' ]
    let isPunc c = System.Char.IsPunctuation(c)
    let isSymbol c = System.Char.IsSymbol(c)
    let isReserved c = reservedChars |> Set.contains c

    let validChars c = (isLetter c || isDigit c || isPunc c || isSymbol c) && not (isReserved c)

    let str s = pstring s

    let parseList : Parser<_> =
        between (str "(") (str ")") (sepBy (many1Satisfy validChars) spaces1)