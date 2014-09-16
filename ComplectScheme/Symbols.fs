module Symbols
    
    type SymbolGenerator() =
        let xs = (Seq.initInfinite (fun i -> i)).GetEnumerator()
        member this.GetNewSymbol() : int =
            xs.MoveNext() |> ignore
            xs.Current

