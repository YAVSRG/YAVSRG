namespace Prelude.Gameplay.Rulesets

open Prelude

module private DP =

    let windows (judge: int) (ridiculous: bool) =
        let pf = 
            if judge >= 9 then
                0.2f * 45.0f<ms> 
            else 45.0f<ms> / 6.0f * (10.0f - (judge |> float32))

        let ma = pf * 0.5f
        let gr = pf * 2f
        let gd = pf * 3f |> min 135.0f<ms>
        let bd = pf * 4f |> min 180.0f<ms>

        let rd = pf * 0.25f

        if ridiculous then
            [
                -bd, 6
                -gd, 5
                -gr, 4
                -pf, 3
                -ma, 2
                -rd, 1
                rd, 0
                ma, 1
                pf, 2
                gr, 3
                gd, 4
                bd, 5
            ]
        else
            [ -bd, 5; -gd, 4; -gr, 3; -pf, 2; -ma, 1; ma, 0; pf, 1; gr, 2; gd, 3; bd, 4 ]