namespace Prelude.Gameplay.Rulesets

open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring

type RulesetComparison =
    {
        Average: float
        Min: float
        Max: float
    }

module RulesetComparison =

    // https://en.wikipedia.org/wiki/Error_function
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

    let simulate (ruleset: Ruleset) (standard_deviation: float) (misses_per_1000_notes: float) : float =
        let non_misses = 1000.0 - misses_per_1000_notes

        let ERF_CONSTANT = standard_deviation * 1.41421356

        let mutable total = 0.0
        let mutable previous_window = 0.0f<ms/rate>
        let mutable previous_proportion = 0.0

        for i, judgement in Seq.indexed ruleset.Judgements do
            match judgement.TimingWindows with
            | Some (early, late) ->
                let early_proportion = 0.5 * erf(float -early / ERF_CONSTANT)
                let late_proportion = 0.5 * erf(float late / ERF_CONSTANT)

                let points =
                    match ruleset.Accuracy with
                    | AccuracyPoints.PointsPerJudgement p -> p.[i]
                    | AccuracyPoints.WifeCurve j ->
                        let delta = 0.5f * (late + previous_window)
                        Wife3Curve.calculate j delta

                total <- total + points * (early_proportion + late_proportion - previous_proportion) * non_misses
                previous_window <- late
                previous_proportion <- early_proportion + late_proportion
            | None -> ()

        let miss_points =
            match ruleset.Accuracy with
            | AccuracyPoints.PointsPerJudgement p -> Array.last p
            | AccuracyPoints.WifeCurve _ -> -2.75

        total <- total + miss_points * non_misses * (1.0 - previous_proportion) // notes that were missed by deviating out of the windows
        total <- total + miss_points * misses_per_1000_notes // notes that were missed because input said so

        total / 10.0 // percent accuracy out of 100

    let standard_deviation_for_accuracy (ruleset: Ruleset) (acc: float) (misses_per_1000_notes: float) : float =
        let mutable sd = 15.0
        let mutable increment = 5.0

        while increment > 0.1 do
            if (simulate ruleset sd misses_per_1000_notes) < acc then
                sd <- sd - increment
            else
                sd <- sd + increment
            increment <- increment * 0.75

        sd

    let compare (accuracy: float) (base_ruleset: Ruleset) (compare_to: Ruleset) : RulesetComparison option =
        let range =
            seq {
                for miss_count in [0.0; 1.0; 3.0; 8.0; 15.0] do
                    let sd = standard_deviation_for_accuracy base_ruleset accuracy miss_count

                    let base_acc = simulate base_ruleset sd miss_count
                    let compare_acc = simulate compare_to sd miss_count

                    if abs (base_acc - accuracy) < 0.11 then
                        yield compare_acc
            }
            |> Array.ofSeq
        if range.Length > 0 then
            Some { Average = Array.average range; Min = Array.min range; Max = Array.max range }
        else
            None