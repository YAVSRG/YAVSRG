﻿namespace Prelude.Charts.Tools.Patterns

open System.Collections.Generic
open Percyqaz.Data
open Prelude
open Prelude.Charts

module PatternSummary =
    
    let ln_percent (chart: Chart) : float32 =
        let mutable notes = 0
        let mutable lnotes = 0
    
        for { Data = nr } in chart.Notes do
            for n in nr do
                if n = NoteType.NORMAL then
                    notes <- notes + 1
                elif n = NoteType.HOLDHEAD then
                    notes <- notes + 1
                    lnotes <- lnotes + 1
    
        float32 lnotes / float32 notes
    
    let sv_time (chart: Chart) : Time =
        if chart.SV.Length = 0 then 0.0f<ms> else
    
        let mutable total = 0.0f<ms>
    
        let mutable time = chart.FirstNote
        let mutable vel = 1.0f
        for sv in chart.SV do
            if not (System.Single.IsFinite vel) || abs(vel - 1.0f) > 0.01f then
                total <- total + (sv.Time - time)
            vel <- sv.Data
            time <- sv.Time
                
        if not (System.Single.IsFinite vel) || abs(vel - 1.0f) > 0.01f then
            total <- total + (chart.LastNote - time)
    
        total

    type private BPMCluster = 
        {
            mutable TotalMs: float32<ms/beat>
            mutable OriginalMsPerBeat: float32<ms/beat>
            mutable Count: int
            mutable BPM: int option
        }
        member this.Add value =
            this.Count <- this.Count + 1
            this.TotalMs <- this.TotalMs + value
        member this.Calculate() = 
            let average = this.TotalMs / float32 this.Count
            this.BPM <- 60000.0f<ms/minute> / average |> float32 |> round |> int |> Some
        member this.Value = this.BPM.Value

    let private BPM_CLUSTER_THRESHOLD = 5.0f<ms/beat>

    type private BPMClusteredPattern = { Time: ScaledTime; BPM: BPMCluster; Density: float32; Mixed: bool }

    let private cluster_pattern_bpms (matched_patterns: Patterns.MatchedCorePattern array) : (PatternId * BPMClusteredPattern) array =
        let clusters = ResizeArray<BPMCluster>()
        let mixed_clusters = Dictionary<PatternId, BPMCluster>()

        let get_cluster value : BPMCluster =
            match clusters |> Seq.tryFind (fun c -> abs (c.OriginalMsPerBeat - value) < BPM_CLUSTER_THRESHOLD) with
            | Some existing_cluster -> 
                existing_cluster.Add value
                existing_cluster
            | None -> 
                let new_cluster = { Count = 1; TotalMs = value; OriginalMsPerBeat = value; BPM = None }
                clusters.Add new_cluster
                new_cluster
        
        let get_mixed_cluster pattern value : BPMCluster =
            if mixed_clusters.ContainsKey pattern then
                let existing_cluster = mixed_clusters.[pattern]
                existing_cluster.Add value
                existing_cluster
            else 
                let new_cluster = { Count = 1; TotalMs = value; OriginalMsPerBeat = value; BPM = None }
                mixed_clusters.Add(pattern, new_cluster)
                new_cluster

        let result = 
            matched_patterns
            |> Array.map (fun info -> 
                (info.Pattern, {
                    Time = info.Time
                    BPM = if info.Mixed then get_mixed_cluster info.Pattern info.MsPerBeat else get_cluster info.MsPerBeat
                    Density = info.Density
                    Mixed = info.Mixed
                }))

        clusters |> Seq.iter (fun cluster -> cluster.Calculate())
        mixed_clusters.Values |> Seq.iter (fun cluster -> cluster.Calculate())

        result

    let private pattern_amount (sorted_times: ScaledTime seq) : ScaledTime =
    
        let PATTERN_DURATION = 600.0f<ms/rate>
        let REST_FALLOFF_FLOOR = 0.5f
        let REST_FALLOFF_RATE = 0.0f // disabled because it works like dog rn

        let mutable amount: ScaledTime = 0.0f<ms/rate>
        let mutable total_time: ScaledTime = 0.0f<ms/rate>

        let mutable current_start = Seq.head sorted_times
        let mutable previous_end = current_start
        let mutable current_end = current_start + PATTERN_DURATION

        let add (rest_time: ScaledTime) (pattern_time: ScaledTime) =
            let falloff_floor = REST_FALLOFF_FLOOR * total_time
            total_time <- total_time + pattern_time
            amount <- pattern_time + max falloff_floor (amount - rest_time * REST_FALLOFF_RATE)

        for time in sorted_times do
            if current_end < time then 
                add (current_start - previous_end) (current_end - current_start)
                current_start <- time
                previous_end <- current_end
                current_end <- current_start + PATTERN_DURATION
            else
                current_end <- time + PATTERN_DURATION
        
        add (current_start - previous_end) (current_end - current_start)

        amount

    let private find_density_percentile (sorted_densities: float32 array) (percentile: float32) =
        let index = percentile * float32 sorted_densities.Length |> floor |> int
        sorted_densities.[index]

    [<Json.AutoCodec>]
    type PatternBreakdown = 
        {
            Pattern: PatternId
            BPM: int
            Mixed: bool
            Amount: ScaledTime
            Density10: float32
            Density25: float32
            Density50: float32
            Density75: float32
            Density90: float32
        }
    
    let private pattern_breakdown (patterns: (PatternId * BPMClusteredPattern) seq) : PatternBreakdown seq =
            
        let groups = patterns |> Array.ofSeq |> Array.groupBy (fun (pattern, info) -> (pattern, info.BPM.Value, info.Mixed))

        seq {
            for ((pattern, bpm, mixed), data) in groups do
                let times = data |> Array.map (fun (_, data) -> data.Time)
                let sorted_data = data |> Array.map (fun (_, data) -> data.Density) |> Array.sort
                let amount = pattern_amount times
    
                yield { 
                    Pattern = pattern
                    BPM = bpm
                    Mixed = mixed
                    Amount = amount
                    Density10 = find_density_percentile sorted_data 0.10f
                    Density25 = find_density_percentile sorted_data 0.25f
                    Density50 = find_density_percentile sorted_data 0.50f
                    Density75 = find_density_percentile sorted_data 0.75f
                    Density90 = find_density_percentile sorted_data 0.90f
                }
        }

    let categorise_chart (patterns: PatternBreakdown list) =
        let is_jacks = fun e -> match e.Pattern with Jack _ -> true | _ -> false
        let is_stream = fun e -> match e.Pattern with Stream "Stream" -> true | _ -> false
        let is_cs = fun e -> match e.Pattern with Stream "Chordstream" -> true | _ -> false
        let jacks = patterns |> List.filter is_jacks
        let streams = patterns |> List.filter is_stream
        let chordstream = patterns |> List.filter is_cs
        let mixed = patterns |> List.filter (_.Mixed)
    
        let total = patterns |> List.sumBy (fun e -> e.Amount)
        let stream_pc = streams |> List.sumBy (fun e -> e.Amount) |> fun x -> x / total
        let jack_pc = jacks |> List.sumBy (fun e -> e.Amount) |> fun x -> x / total
        let cs_pc = chordstream |> List.sumBy (fun e -> e.Amount) |> fun x -> x / total
        let mixed_pc = mixed |> List.sumBy (fun e -> e.Amount) |> fun x -> x / total
    
        if cs_pc > 0.5f then
            "JS/HS"
        elif mixed_pc > 0.5f then
            "Stream tech"
        elif jack_pc > 0.8f then
            "Chordjacks"
        elif jack_pc > 0.3f && stream_pc > 0.3f && mixed_pc > 0.3f then 
            "Hybrid tech"
        elif stream_pc > 0.3f && jack_pc > 0.3f then
            "Hybrid"
        elif stream_pc > 0.5f && mixed_pc < 0.2f then
            "Speed"
        else "Unknown"

    [<Json.AutoCodec>]
    type PatternDetailsReport = { Patterns: PatternBreakdown list; LNPercent: float32; SVAmount: Time; Category: string }
    
    let generate_detailed_pattern_data (rate: float32, chart: Chart) : PatternDetailsReport =
        let breakdown = 
            Patterns.analyse rate chart
            |> cluster_pattern_bpms
            |> Seq.filter (fun (_, info) -> info.BPM.Value >= 85)
            |> pattern_breakdown
            |> Seq.sortByDescending (fun x -> x.Amount)
            |> List.ofSeq
            |> List.truncate 16

        let is_useless (pattern: PatternBreakdown) =
            breakdown |> Seq.exists (fun p -> p.Pattern = pattern.Pattern && p.Amount * 0.5f > pattern.Amount && p.BPM > pattern.BPM && p.Mixed = pattern.Mixed)

        let patterns = breakdown |> List.filter (is_useless >> not)

        {
            Patterns = patterns
            LNPercent = ln_percent chart
            SVAmount = sv_time chart
            Category = categorise_chart patterns
        }
    
    let generate_cached_pattern_data (rate: float32, chart: Chart) : PatternDetailsReport =
         let breakdown = 
             Patterns.analyse rate chart
             |> cluster_pattern_bpms
             |> Seq.filter (fun (_, info) -> info.BPM.Value >= 85)
             |> pattern_breakdown
             |> Seq.sortByDescending (fun x -> x.Amount)
             |> List.ofSeq
             |> List.truncate 16

         let is_useless (pattern: PatternBreakdown) =
             breakdown |> Seq.exists (fun p -> p.Pattern = pattern.Pattern && p.Amount * 0.5f > pattern.Amount && p.BPM > pattern.BPM && p.Mixed = pattern.Mixed)

         let patterns = breakdown |> List.filter (is_useless >> not) |> List.truncate 6

         {
             Patterns = patterns
             LNPercent = ln_percent chart
             SVAmount = sv_time chart
             Category = categorise_chart patterns
         }