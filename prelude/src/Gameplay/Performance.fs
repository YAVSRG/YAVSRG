namespace Prelude.Gameplay

open System
open Prelude
open Prelude.Charts.Processing.Difficulty

module Performance =
    
    let private confidence_value (delta: Time) =
        let delta = float delta
    
        let phi x =
            let y = x / 1.414213562
    
            Math.Max(
                0.0,
                Math.Min(
                    (0.5
                        + Math.Pow(Math.PI, -0.5)
                        * (y - Math.Pow(y, 3.0) / 3.0 + Math.Pow(y, 5.0) / 10.0 - Math.Pow(y, 7.0) / 42.0
                            + Math.Pow(y, 9.0) / 216.0)),
                    1.0
                )
            )
    
        phi ((17.95 - Math.Max(2.0, Math.Abs delta)) / 15.0)
    
    let private performance_func b value deviation delta =
        DifficultyRating.stamina_func b (value * confidence_value deviation) delta

    let calculate (rr: DifficultyRating) (keys: int) (scoring: IScoreMetric) =
        let lastTimes = Array.create keys 0.0f<ms>
        let mutable pv = 0.01
        let mutable tv = 0.01
        let pvs = Array.zeroCreate keys
        let tvs = Array.zeroCreate keys

        for (i, (time, deltas, hit)) in Array.indexed scoring.HitData do
            let mutable p = 0.0
            let mutable t = 0.0
            let mutable c = 0.0

            for k = 0 to (keys - 1) do
                if hit.[k] = HitStatus.HIT_ACCEPTED then
                    pvs.[k] <-
                        performance_func (pvs.[k]) (rr.PhysicalComposite.[i, k]) (deltas.[k]) (time - lastTimes.[k])

                    tvs.[k] <-
                        performance_func (tvs.[k]) (rr.TechnicalComposite.[i, k]) (deltas.[k]) (time - lastTimes.[k])

                    lastTimes.[k] <- time
                    c <- c + 1.0
                    p <- p + pvs.[k]
                    t <- t + tvs.[k]

            let p, t = if c = 0.0 then 0.0, 0.0 else p / c, t / c
            pv <- pv * Math.Exp(0.01 * Math.Max(0.0, Math.Log(p / pv)))
            tv <- tv * Math.Exp(0.01 * Math.Max(0.0, Math.Log(t / tv)))

        (Math.Pow(pv, 0.6) * 2.5), (tv)
