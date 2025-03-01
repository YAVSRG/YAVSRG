namespace Prelude.Calculator

open System
open Prelude
open Prelude.Charts
open Prelude.Gameplay.Scoring

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
        Difficulty.stamina_func b (value * float32 (confidence_value hit_delta) / 0.55f) time_delta

    let calculate (rr: Difficulty) (keys: int) (scoring: ScoreProcessor) =
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
                    performance_func (pvs.[ev.Column]) (Difficulty.note_strain_v0 rr.NoteDifficulty.[ev.Index].[ev.Column]) e.Delta ((ev.Time - last_times.[ev.Column]) / scoring.Rate)

                last_times.[ev.Column] <- ev.Time
                c <- c + 1.0f
                p <- p + pvs.[ev.Column]

                let p = if c = 0.0f then 0.0f else p / c
                pv <- pv * MathF.Exp(0.01f * Math.Max(0.0f, MathF.Log(p / pv)))

            | _ -> ()

        MathF.Pow(pv, Difficulty.CURVE_POWER) * Difficulty.CURVE_SCALE
        |> float

    let acc_timeline (rr: Difficulty) (scoring: ScoreProcessor) =
        let mutable v = 1.0
        let mutable i = 0
        let output : float array = Array.zeroCreate rr.NoteDifficulty.Length
        // todo: dropped holds have another calculation
        for ev in scoring.Events do
            while ev.Index > i do
                output.[i] <- v
                i <- i + 1
            match ev.Action.Judgement with
            | Some (_, value) ->
                v <- 0.95 * v + 0.05 * value
            | None -> ()
        output.[output.Length - 1] <- v

        //seq {
        //    yield sprintf "Strain, Accuracy"
        //    for i = 0 to output.Length - 1 do
        //        yield sprintf "%.2f, %.2f" (snd rr.Strain.[i]) (output.[i] * 100.0)
        //}
        //|> fun lines -> System.IO.File.WriteAllLines("plot.csv", lines)

        output

    let acc_vs_difficulty (lo: float32) (rr: Difficulty) (scoring: ScoreProcessor) =

        try
            seq {
                for ev in scoring.Events do

                    if rr.Strains.[ev.Index].StrainV1Notes.[ev.Column] >= lo then
                        match ev.Action.Judgement with
                        | Some (_, value) -> yield value
                        | None -> ()
            }
            |> Seq.average
        with _ -> 0.0

    let acc_vs_difficulty_graph (rr: Difficulty) (scoring: ScoreProcessor) =

        let strain_data = rr.Strains |> Seq.map _.StrainV1Notes |> Seq.concat |> Seq.sort |> Array.ofSeq
        seq {
            for percentile = 1 to 100 do

                let index_lo = (float32 percentile / 100.0f) * float32 strain_data.Length |> round |> int |> max 0 |> min (strain_data.Length - 1)
                let threshold_lo = strain_data.[index_lo]

                yield percentile, acc_vs_difficulty threshold_lo rr scoring
        }
        |> Array.ofSeq