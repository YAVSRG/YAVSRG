namespace Prelude.Calculator

module Layout =

    let keys_on_left_hand (keymode: int) =
        match keymode with
        | 3 -> 2
        | 4 -> 2
        | 5 -> 3
        | 6 -> 3
        | 7 -> 4
        | 8 -> 4
        | 9 -> 5
        | 10 -> 5
        | _ -> failwithf "Invalid keymode %i" keymode