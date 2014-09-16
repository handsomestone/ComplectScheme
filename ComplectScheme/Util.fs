module Util

    let someOrNull =
        function
            | Some (t) -> t
            | None -> null

    let apply f x =
        f x

    type State<'a, 's> = State of ('s -> 'a * 's)

    let runState (State s) a = s a
    let getState = State (fun s -> (s,s))
    let putState s = State (fun _ -> ((),s))

    type StateBuilder() =
        member this.Return(a) = 
            State (fun s -> (a,s))
        member this.Bind(m,k) =
            State (fun s -> 
                let (a,s') = runState m s 
                runState (k a) s')
        member this.ReturnFrom (m) = m
