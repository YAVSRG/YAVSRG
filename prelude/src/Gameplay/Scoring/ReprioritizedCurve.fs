namespace Prelude.Gameplay.Scoring

open Prelude

/// Etterna's Wife3 curve mapping milliseconds offset to points
/// Lifted from https://github.com/etternagame/etterna/blob/0a7bd768cffd6f39a3d84d76964097e43011ce33/Themes/_fallback/Scripts/10%20Scores.lua#L606-L627
module ReprioritizedCurve =

    // Approximates https://en.wikipedia.org/wiki/Error_function
    // idk why Mina used this

    let private curve =

        fun (x: float) ->
            let sign = if x < 0.0 then -1.0 else 1.0
            let x = abs x

            let y = 
                1.0 - ((-x) * 0.02) ** 1.5
            sign * y

    let calculate (judge: float32) (delta: GameplayTime) : float =

        let delta = float delta |> abs

        let scale = (10.0 - float judge) / 6.0
        let miss_weight = 0
        let ridic = 11.25 * scale
        let boo_window = 180.0 * scale
        let ts_pow = 0.75
        let zero = 65.0 * (scale ** ts_pow)
        let dev = 22.7 * (scale ** ts_pow)

        if delta <= ridic then // 100% window
            1.0
        elif delta <= zero then // curve window?
            curve ((zero - delta) / dev)
        elif delta <= boo_window then // overridden window ?
            -1
        else
            miss_weight
