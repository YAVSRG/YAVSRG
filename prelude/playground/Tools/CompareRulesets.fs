module CompareRulesets

open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring

// https://en.wikipedia.org/wiki/Error_function
let erf =
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

let simulate_ruleset (ruleset: Ruleset) (standard_deviation: float) (misses_per_1000_notes: float) =
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

let tabulate_ruleset (ruleset: Ruleset) =

    printfn ""
    printfn "%s\t|\t0\t|\t1\t|\t3\t|\t8\t|\t25\t| <- misses per 1000 notes" (ruleset.Name.Replace(" ", ""))
    printfn "vv SD\t|\t-\t|\t-\t|\t-\t|\t-\t|\t--\t|"

    for sd in [11.0; 13.0; 16.0; 20.0; 25.0] do
        printf "%.0fms\t|\t" sd

        for miss_count in [0.0; 1.0; 3.0; 8.0; 15.0] do
            printf "%.2f%%\t|\t" (simulate_ruleset ruleset sd miss_count)

        printfn ""

let find_sd_for_acc (ruleset: Ruleset) (acc: float) (misses_per_1000_notes: float) =
    let mutable sd = 15.0
    let mutable increment = 5.0

    while increment > 0.1 do
        if (simulate_ruleset ruleset sd misses_per_1000_notes) < acc then
            sd <- sd - increment
        else
            sd <- sd + increment
        increment <- increment * 0.75

    sd

let compare_rulesets (accuracy: float) (base_ruleset: Ruleset) (compare_to: Ruleset) =
    let range =
        seq {
            for miss_count in [0.0; 1.0; 3.0; 8.0; 15.0] do
                let sd = find_sd_for_acc base_ruleset accuracy miss_count

                let base_acc = simulate_ruleset base_ruleset sd miss_count
                let compare_acc = simulate_ruleset compare_to sd miss_count

                if abs (base_acc - accuracy) < 0.11 then
                    yield compare_acc
        }
        |> Array.ofSeq
    if range.Length > 0 then
        printfn "%.1f%% on %s is about %.1f%% (%.1f%% - %.1f%%) on %s" accuracy base_ruleset.Name (Array.average range) (Array.min range) (Array.max range) compare_to.Name
    else
        printfn "Failed to decently compare %.1f%% on %s to an equivalent on %s" accuracy base_ruleset.Name compare_to.Name

let multi_compare (base_ruleset: Ruleset) (compare_to: Ruleset) =

    compare_rulesets 91.0 base_ruleset compare_to
    compare_rulesets 92.0 base_ruleset compare_to
    compare_rulesets 93.0 base_ruleset compare_to
    compare_rulesets 94.0 base_ruleset compare_to
    compare_rulesets 95.0 base_ruleset compare_to
    compare_rulesets 96.0 base_ruleset compare_to
    compare_rulesets 97.0 base_ruleset compare_to
    compare_rulesets 98.0 base_ruleset compare_to
    printfn ""

let main() =

    let sc_4 = SC.create 4
    let osu_8 = OsuMania.create 8.0f OsuMania.NoMod
    let osu_5 = OsuMania.create 5.0f OsuMania.NoMod
    let wife_4 = Wife3.create 4
    let wife_5 = Wife3.create 5

    multi_compare sc_4 sc_4
    multi_compare sc_4 osu_8
    multi_compare sc_4 osu_5
    multi_compare sc_4 wife_4
    multi_compare sc_4 wife_5
    multi_compare wife_4 wife_5
    multi_compare osu_8 osu_5