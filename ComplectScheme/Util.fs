module Util

    let someOrNull =
        function
            | Some (t) -> t
            | None -> null