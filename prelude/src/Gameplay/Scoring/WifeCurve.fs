namespace Prelude.Gameplay.Scoring

open Prelude

/// Etterna's Wife3 curve mapping milliseconds offset to points
/// Lifted from https://github.com/etternagame/etterna/blob/0a7bd768cffd6f39a3d84d76964097e43011ce33/Themes/_fallback/Scripts/10%20Scores.lua#L606-L627
module Wife3Curve =

    // Approximates https://en.wikipedia.org/wiki/Error_function
    // idk why Mina used this

    let private erf =
        let a1 = 0.254829592
        let a2 = -0.284496736
        let a3 = 1.421413741
        let a4 = -1.453152027
        let a5 = 1.061405429
        let p = 0.3275911

        fun (x: float) ->
            let sign = if x < 0.0 then -1.0 else 1.0
            let x = abs x
            let t = 1.0 / (1.0 + p * x)

            let y =
                1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * exp(-x * x)

            sign * y

    let calculate (judge: int) (delta: GameplayTime) : float =

        let delta = float delta |> abs

        // Max points per note in Etterna is 2, in Interlude it's 1
        // Hence there is no multiplication by 2 and `miss_weight` is -2.75 instead of -5.5

        let scale = (10.0 - float judge) / 6.0
        let miss_weight = -2.75
        let ridic = 5.0 * scale
        let boo_window = 180.0 * scale
        let ts_pow = 0.75
        let zero = 65.0 * (scale ** ts_pow)
        let dev = 22.7 * (scale ** ts_pow)

        if delta <= ridic then
            1.0
        elif delta <= zero then
            erf ((zero - delta) / dev)
        elif delta <= boo_window then
            (delta - zero) * miss_weight / (boo_window - zero)
        else
            miss_weight