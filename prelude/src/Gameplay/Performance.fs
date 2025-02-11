namespace Prelude.Gameplay

open System
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Charts.Processing.Difficulty

module Performance =

    let private confidence_value (delta: GameplayTime) =
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

    let private performance_func b value hit_delta (time_delta: GameplayTime) =
        DifficultyRating.stamina_func b (value * float32 (confidence_value hit_delta)) time_delta

    let calculate (rr: DifficultyRating) (keys: int) (scoring: ScoreProcessor) =
        let last_times = Array.create keys 0.0f<ms>
        let mutable pv = 0.01f
        let pvs = Array.zeroCreate keys

        for ev in scoring.Events do
            match ev.Action with
            | Hit e
            | Hold e ->

                let mutable p = 0.0f
                let mutable c = 0.0f

                pvs.[ev.Column] <-
                    performance_func (pvs.[ev.Column]) (rr.NoteDifficulty.[ev.Index].[ev.Column].T) e.Delta ((ev.Time - last_times.[ev.Column]) / scoring.Rate)

                last_times.[ev.Column] <- ev.Time
                c <- c + 1.0f
                p <- p + pvs.[ev.Column]

                let p = if c = 0.0f then 0.0f else p / c
                pv <- pv * MathF.Exp(0.01f * Math.Max(0.0f, MathF.Log(p / pv)))

            | _ -> ()

        MathF.Pow(pv, DifficultyRating.CURVE_POWER) * DifficultyRating.CURVE_SCALE
        |> float