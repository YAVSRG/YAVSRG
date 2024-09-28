namespace Prelude.Charts.Processing.Patterns

open Percyqaz.Data
open Prelude

[<Json.AutoCodec>]
type PatternBreakdown =
    {
        Pattern: CorePatternType
        BPM: int
        Mixed: bool
        Amount: GameplayTime
        Density10: float32
        Density25: float32
        Density50: float32
        Density75: float32
        Density90: float32
        Specifics: (string * int) array
    }
    static member Default =
        { 
            Pattern = Jack
            BPM = 100
            Mixed = false
            Amount = 0.0f<ms/rate>
            Density10 = 0.0f
            Density25 = 0.0f
            Density50 = 0.0f
            Density75 = 0.0f
            Density90 = 0.0f
            Specifics = [||]
        }

module private Breakdown =

    let private pattern_amount (sorted_times: GameplayTime seq) : GameplayTime =

        let PATTERN_DURATION = 800.0f<ms / rate>

        let mutable total_time: GameplayTime = 0.0f<ms / rate>

        let mutable current_start = Seq.head sorted_times
        let mutable current_end = current_start + PATTERN_DURATION

        for time in sorted_times do
            if current_end < time then
                total_time <- total_time + (current_end - current_start)

                current_start <- time
                current_end <- current_start + PATTERN_DURATION
            else
                current_end <- time + PATTERN_DURATION

        total_time <- total_time +  (current_end - current_start)

        total_time

    let generate
        (specific_patterns: MatchedSpecificPattern array)
        (patterns: (CorePatternType * BPMClusteredPattern) seq)
        : PatternBreakdown seq =

        let groups =
            patterns
            |> Array.ofSeq
            |> Array.groupBy (fun (pattern, info) -> (pattern, info.BPM.Value, info.Mixed))

        seq {
            for ((pattern, bpm, mixed), data) in groups do
                let times = data |> Array.map (fun (_, data) -> data.Time)

                let sorted_densities =
                    data |> Array.map (fun (_, data) -> data.Density) |> Array.sort

                let amount = pattern_amount times

                let approx_mspb = 60000.0f<ms> / (float32 bpm * 1.0f<beat>)

                let matching_specifics =
                    specific_patterns
                    |> Array.filter (fun x ->
                        fst x.Pattern = pattern
                        && abs (x.MsPerBeat - approx_mspb) < Clustering.BPM_CLUSTER_THRESHOLD
                    )
                    |> Array.map (fun x -> snd x.Pattern)
                    |> Array.countBy id
                    |> Array.sortByDescending snd
                    |> Array.truncate 3

                yield
                    {
                        Pattern = pattern
                        BPM = bpm
                        Mixed = mixed
                        Amount = amount
                        Density10 = Density.find_percentile sorted_densities 0.10f
                        Density25 = Density.find_percentile sorted_densities 0.25f
                        Density50 = Density.find_percentile sorted_densities 0.50f
                        Density75 = Density.find_percentile sorted_densities 0.75f
                        Density90 = Density.find_percentile sorted_densities 0.90f
                        Specifics = matching_specifics
                    }
        }