namespace Prelude.Calculator.Patterns

open System.Collections.Generic
open Percyqaz.Data
open Prelude
open Prelude.Calculator

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

        Rating: float32

        Density10: Density
        Density25: Density
        Density50: Density
        Density75: Density
        Density90: Density

        Amount: Time
    }

    static member Default =
        {
            Pattern = Jacks
            SpecificTypes = []
            BPM = 120<beat / minute / rate>
            Mixed = false

            Rating = 0.0f

            Density10 = 0.0f</rate>
            Density25 = 0.0f</rate>
            Density50 = 0.0f</rate>
            Density75 = 0.0f</rate>
            Density90 = 0.0f</rate>

            Amount = 1.0f<ms>
        }

    member this.Importance =
        this.Amount * this.Pattern.RatingMultiplier * float32 this.BPM

    member this.Format (rate: Rate) =

        let name =
            match this.SpecificTypes with
            | (t, amount) :: _ when amount > 0.4f -> t
            | _ -> this.Pattern.ToString()

        if this.Mixed then
            sprintf "~%.0fBPM Mixed %s" (float32 this.BPM * rate) name
        else
            sprintf "%.0fBPM %s" (float32 this.BPM * rate) name

module private Clustering =

    let BPM_CLUSTER_THRESHOLD = 5.0f<ms / beat>

    let find_percentile (percentile: float32) (sorted_values: float32<'u> array) : float32<'u> =
        if sorted_values.Length = 0 then 0.0f |> LanguagePrimitives.Float32WithMeasure else
        let index = percentile * float32 sorted_values.Length |> floor |> int
        sorted_values.[index]

    let private pattern_amount (sorted_starts_ends: (Time * Time) array) : Time =

        let mutable total_time: Time = 0.0f<ms>

        let a, b = Array.head sorted_starts_ends

        let mutable current_start = a
        let mutable current_end = b

        for start, _end in sorted_starts_ends do
            if current_end < _end then
                total_time <- total_time + (current_end - current_start)

                current_start <- start
                current_end <- _end
            else
                current_end <- max current_end _end

        total_time <- total_time +  (current_end - current_start)

        total_time

    let assign_clusters (patterns: FoundPattern array) : (FoundPattern * ClusterBuilder) array =
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
            patterns
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

    let specific_clusters (patterns_with_clusters: (FoundPattern * ClusterBuilder) array) : Cluster array =

        patterns_with_clusters
        |> Array.groupBy (fun (pattern, c) ->
            pattern.Pattern, pattern.Mixed, c.Value
        )
        |> Array.map (fun ((pattern, mixed, bpm), data) ->
            let starts_ends = data |> Array.map (fun (m, _) -> m.Start, m.End)
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

                Rating = data |> Seq.map (fst >> _.Strains) |> Seq.concat |> Seq.filter (fun x -> x > 0.0f) |> Difficulty.weighted_overall_difficulty

                Density10 = find_percentile 0.1f densities
                Density25 = find_percentile 0.25f densities
                Density50 = find_percentile 0.5f densities
                Density75 = find_percentile 0.75f densities
                Density90 = find_percentile 0.9f densities

                Amount = pattern_amount starts_ends
            }
        )

    let core_pattern_cluster (pattern_type: CorePattern) (patterns_with_clusters: (FoundPattern * ClusterBuilder) array) : Cluster =

        let data =
            patterns_with_clusters
            |> Array.filter (fun (pattern, _) -> pattern.Pattern = pattern_type)

        let starts_ends = data |> Array.map (fun (m, _) -> m.Start, m.End)
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
            Pattern = pattern_type
            SpecificTypes = specific_types
            // todo: type ClusterType = | Normal of bpm: int | Mixed of bpm: int | Combined
            BPM = 0<beat / minute / rate>
            Mixed = true

            Rating = data |> Seq.map (fst >> _.Strains) |> Seq.concat |> Seq.filter (fun x -> x > 0.0f) |> Difficulty.weighted_overall_difficulty

            Density10 = find_percentile 0.1f densities
            Density25 = find_percentile 0.25f densities
            Density50 = find_percentile 0.5f densities
            Density75 = find_percentile 0.75f densities
            Density90 = find_percentile 0.9f densities

            Amount = pattern_amount starts_ends
        }

    let calculate_clustered_patterns (patterns: FoundPattern array) : Cluster array =
        let patterns_with_clusters = assign_clusters patterns
        let specific_clusters = specific_clusters patterns_with_clusters
        specific_clusters
        //let core_patterns =
        //    [| Jacks; Chordstream; Stream; |]
        //    |> Array.map (fun pattern_type -> core_pattern_cluster pattern_type patterns_with_clusters)
        //Array.append specific_clusters core_patterns