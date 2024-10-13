namespace Prelude.Charts.Processing.Patterns

open System.Collections.Generic
open Percyqaz.Data
open Prelude

type private ClusterBuilder =
    internal {
        mutable SumMs: float32<ms / beat>
        OriginalMsPerBeat: float32<ms / beat>
        mutable Count: int
        mutable BPM: int<beat / minute / rate> option
    }
    member this.Add value =
        this.Count <- this.Count + 1
        this.SumMs <- this.SumMs + value

    member this.Calculate() =
        let average = this.SumMs / float32 this.Count
        this.BPM <- 60000.0f<ms / minute> / average |> float32 |> round |> int |> fun x -> x * 1<beat / minute / rate> |> Some

    member this.Value = this.BPM.Value

[<Json.AutoCodec>]
type Cluster =
    {
        Pattern: CorePattern
        SpecificTypes: (string * float32) list
        BPM: int<beat / minute / rate>
        Mixed: bool

        Density10: Density
        Density25: Density
        Density50: Density
        Density75: Density
        Density90: Density

        Amount: Time

        Time10: Time
        Time50: Time
        Time90: Time
    }

    static member Default =
        {
            Pattern = Jacks
            SpecificTypes = []
            BPM = 120<beat / minute / rate>
            Mixed = false

            Density10 = 0.0f</rate>
            Density25 = 0.0f</rate>
            Density50 = 0.0f</rate>
            Density75 = 0.0f</rate>
            Density90 = 0.0f</rate>

            Amount = 1.0f<ms>

            Time10 = 0.0f<ms>
            Time50 = 1.0f<ms>
            Time90 = 2.0f<ms>
        }

    member this.Importance =
        this.Amount * this.Pattern.RatingMultiplier * float32 this.BPM

    override this.ToString() =

        let name =
            match this.SpecificTypes with
            | (t, amount) :: _ when amount > 0.4f -> t
            | _ -> this.Pattern.ToString()

        if this.Mixed then
            sprintf "~%iBPM Mixed %s" this.BPM name
        else
            sprintf "%iBPM %s" this.BPM name

module private Clustering =

    let BPM_CLUSTER_THRESHOLD = 5.0f<ms / beat>
    
    let find_percentile (percentile: float32) (sorted_values: float32<'u> array) : float32<'u> =
        if sorted_values.Length = 0 then 0.0f |> LanguagePrimitives.Float32WithMeasure else
        let index = percentile * float32 sorted_values.Length |> floor |> int
        sorted_values.[index]

    let private pattern_amount (sorted_times: Time array) : Time =

        // todo: scale this inversely off the bpm of the cluster for consistency-over-rates
        let PATTERN_DURATION = 800.0f<ms>

        let mutable total_time: Time = 0.0f<ms>

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

    let calculate_clustered_patterns (matched_patterns: MatchedPattern array) : Cluster array =
        let bpms_non_mixed = ResizeArray<ClusterBuilder>()
        let bpms_mixed = Dictionary<CorePattern, ClusterBuilder>()

        let add_to_cluster ms_per_beat : ClusterBuilder =
            match
                bpms_non_mixed
                |> Seq.tryFind (fun c -> abs (c.OriginalMsPerBeat - ms_per_beat) < BPM_CLUSTER_THRESHOLD)
            with
            | Some existing_cluster ->
                existing_cluster.Add ms_per_beat
                existing_cluster
            | None ->
                let new_cluster =
                    {
                        Count = 1
                        SumMs = ms_per_beat
                        OriginalMsPerBeat = ms_per_beat
                        BPM = None
                    }

                bpms_non_mixed.Add new_cluster
                new_cluster

        let add_to_mixed_cluster pattern value : ClusterBuilder =
            if bpms_mixed.ContainsKey pattern then
                let existing_cluster = bpms_mixed.[pattern]
                existing_cluster.Add value
                existing_cluster
            else
                let new_cluster =
                    {
                        Count = 1
                        SumMs = value
                        OriginalMsPerBeat = value
                        BPM = None
                    }

                bpms_mixed.Add(pattern, new_cluster)
                new_cluster

        let patterns_with_clusters = 
            matched_patterns
            |> Array.map (fun pattern ->
                pattern, 
                if pattern.Mixed then
                    add_to_mixed_cluster pattern.Pattern pattern.MsPerBeat
                else
                    add_to_cluster pattern.MsPerBeat
            )

        bpms_non_mixed |> Seq.iter (fun cluster -> cluster.Calculate())
        bpms_mixed.Values |> Seq.iter (fun cluster -> cluster.Calculate())

        patterns_with_clusters
        |> Array.groupBy (fun (pattern, c) ->
            pattern.Pattern, pattern.Mixed, c.Value
        )
        |> Array.map (fun ((pattern, mixed, bpm), data) ->
            let times = data |> Array.map (fst >> _.Time)
            let densities = data |> Array.map (fst >> _.Density) |> Array.sort

            let data_count = float32 data.Length
            let specific_types =
                data 
                |> Array.choose (fst >> _.SpecificType)
                |> Array.countBy id
                |> Seq.map (fun (specific_type, count) -> (specific_type, float32 count / data_count))
                |> Seq.sortByDescending snd
                |> List.ofSeq

            {
                Pattern = pattern
                SpecificTypes = specific_types
                BPM = bpm
                Mixed = mixed
                
                Density10 = find_percentile 0.1f densities
                Density25 = find_percentile 0.25f densities
                Density50 = find_percentile 0.5f densities
                Density75 = find_percentile 0.75f densities
                Density90 = find_percentile 0.9f densities

                Amount = pattern_amount times

                Time10 = find_percentile 0.1f times
                Time50 = find_percentile 0.5f times
                Time90 = find_percentile 0.9f times
            }
        )