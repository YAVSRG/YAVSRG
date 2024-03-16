namespace Prelude.Charts.Processing.Patterns

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
        if chart.SV.Length = 0 then
            0.0f<ms>
        else

            let mutable total = 0.0f<ms>

            let mutable time = chart.FirstNote
            let mutable vel = 1.0f

            for sv in chart.SV do
                if not (System.Single.IsFinite vel) || abs (vel - 1.0f) > 0.01f then
                    total <- total + (sv.Time - time)

                vel <- sv.Data
                time <- sv.Time

            if not (System.Single.IsFinite vel) || abs (vel - 1.0f) > 0.01f then
                total <- total + (chart.LastNote - time)

            total

    type private BPMCluster =
        {
            mutable TotalMs: float32<ms / beat>
            mutable OriginalMsPerBeat: float32<ms / beat>
            mutable Count: int
            mutable BPM: int option
        }
        member this.Add value =
            this.Count <- this.Count + 1
            this.TotalMs <- this.TotalMs + value

        member this.Calculate() =
            let average = this.TotalMs / float32 this.Count
            this.BPM <- 60000.0f<ms / minute> / average |> float32 |> round |> int |> Some

        member this.Value = this.BPM.Value

    let private BPM_CLUSTER_THRESHOLD = 5.0f<ms / beat>

    type private BPMClusteredPattern =
        {
            Time: ScaledTime
            BPM: BPMCluster
            Density: float32
            Mixed: bool
        }

    let private cluster_pattern_bpms
        (matched_patterns: Patterns.MatchedCorePattern array)
        : (CorePatternType * BPMClusteredPattern) array =
        let clusters = ResizeArray<BPMCluster>()
        let mixed_clusters = Dictionary<CorePatternType, BPMCluster>()

        let get_cluster value : BPMCluster =
            match
                clusters
                |> Seq.tryFind (fun c -> abs (c.OriginalMsPerBeat - value) < BPM_CLUSTER_THRESHOLD)
            with
            | Some existing_cluster ->
                existing_cluster.Add value
                existing_cluster
            | None ->
                let new_cluster =
                    {
                        Count = 1
                        TotalMs = value
                        OriginalMsPerBeat = value
                        BPM = None
                    }

                clusters.Add new_cluster
                new_cluster

        let get_mixed_cluster pattern value : BPMCluster =
            if mixed_clusters.ContainsKey pattern then
                let existing_cluster = mixed_clusters.[pattern]
                existing_cluster.Add value
                existing_cluster
            else
                let new_cluster =
                    {
                        Count = 1
                        TotalMs = value
                        OriginalMsPerBeat = value
                        BPM = None
                    }

                mixed_clusters.Add(pattern, new_cluster)
                new_cluster

        let result =
            matched_patterns
            |> Array.map (fun info ->
                (info.Pattern,
                 {
                     Time = info.Time
                     BPM =
                         if info.Mixed then
                             get_mixed_cluster info.Pattern info.MsPerBeat
                         else
                             get_cluster info.MsPerBeat
                     Density = info.Density
                     Mixed = info.Mixed
                 })
            )

        clusters |> Seq.iter (fun cluster -> cluster.Calculate())
        mixed_clusters.Values |> Seq.iter (fun cluster -> cluster.Calculate())

        result

    let private pattern_amount (sorted_times: ScaledTime seq) : ScaledTime =

        let PATTERN_DURATION = 600.0f<ms / rate>
        let REST_FALLOFF_FLOOR = 0.5f
        let REST_FALLOFF_RATE = 0.0f // disabled because it works like dog rn

        let mutable amount: ScaledTime = 0.0f<ms / rate>
        let mutable total_time: ScaledTime = 0.0f<ms / rate>

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
            Pattern: CorePatternType
            BPM: int
            Mixed: bool
            Amount: ScaledTime
            Density10: float32
            Density25: float32
            Density50: float32
            Density75: float32
            Density90: float32
            Specifics: (string * int) array
        }

    let private pattern_breakdown
        (specific_patterns: Patterns.MatchedSpecificPattern array)
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
                        && abs (x.MsPerBeat - approx_mspb) < BPM_CLUSTER_THRESHOLD
                    )
                    |> Array.map (fun x -> snd x.Pattern)
                    |> Array.countBy id
                    //|> Array.filter (fun (_, count) -> count > times.Length / 100)
                    |> Array.sortByDescending snd
                    |> Array.truncate 3

                yield
                    {
                        Pattern = pattern
                        BPM = bpm
                        Mixed = mixed
                        Amount = amount
                        Density10 = find_density_percentile sorted_densities 0.10f
                        Density25 = find_density_percentile sorted_densities 0.25f
                        Density50 = find_density_percentile sorted_densities 0.50f
                        Density75 = find_density_percentile sorted_densities 0.75f
                        Density90 = find_density_percentile sorted_densities 0.90f
                        Specifics = matching_specifics
                    }
        }

    [<Json.AutoCodec>]
    type ChartCategorisation =
        {
            Category: string
            MajorFeatures: string list
            MinorFeatures: string list
        }
        static member Default = { Category = "Unknown"; MajorFeatures = []; MinorFeatures = [] }

    // todo: make into a union for easier reasoning
    type private CategoryFragment =
        {
            CorePattern: CorePatternType
            Mixed: bool // todo: turn 3-pronged with mixed, non mixed, combo of both
            BPM: int option
            Specific: string option
            Importance: ScaledTime
        }
        override this.ToString() =
            match this.Specific with
            | Some spec ->
                if this.Mixed then 
                    sprintf "~%iBPM Mixed %s" this.BPM.Value spec 
                else 
                    sprintf "%iBPM %s" this.BPM.Value spec
            | None ->
                match this.BPM with
                | None ->
                    match this.CorePattern with
                    | Stream -> "Streams"
                    | Chordstream -> "Chordstream"
                    | Jack -> "Jacks"
                | Some bpm ->
                    match this.CorePattern with
                    | Stream ->
                        if this.Mixed then
                            sprintf "~%iBPM Mixed Streams" bpm
                        else
                            sprintf "%iBPM Streams" bpm
                    | Chordstream ->
                        if this.Mixed then
                            sprintf "~%iBPM Mixed Chordstream" bpm
                        else
                            sprintf "%iBPM Chordstream" bpm
                    | Jack -> 
                        if this.Mixed then
                            sprintf "~%iBPM Mixed Jacks" bpm
                        else
                            sprintf "%iBPM Jacks" bpm
        member this.MoreUsefulThan(f: CategoryFragment) =
            if f.CorePattern <> this.CorePattern then false else

            (f.BPM.IsNone && this.BPM.IsSome)
            || (f.Specific.IsNone && this.Specific.IsSome)

    let categorise_chart (keys: int) (patterns: PatternBreakdown list) : ChartCategorisation =

        let total = 0.01f<ms/rate> + (patterns |> List.sumBy (fun e -> e.Amount))
        let average_density = (patterns |> List.sumBy (fun e -> e.Density50 * e.Amount)) / total

        let importance (density: float32) (amount: float32<ms/rate>) =
            density / average_density * amount

        let fragments =
            seq {
                for p in patterns do
                    let total_specs = p.Specifics |> Seq.sumBy snd

                    for spec, count in p.Specifics do
                        let spec_amount = p.Amount * float32 count / float32 total_specs
                        yield {
                            CorePattern = p.Pattern
                            Mixed = p.Mixed
                            BPM = Some p.BPM
                            Specific = Some spec
                            Importance = importance p.Density50 spec_amount
                        }

                for p in patterns do
                    yield {
                        CorePattern = p.Pattern
                        Mixed = p.Mixed
                        BPM = Some p.BPM
                        Specific = None
                        Importance = importance p.Density50 p.Amount
                    }

                let backup_for_core_pattern (c: CorePatternType) =
                    let ps = patterns |> List.filter (fun e -> e.Pattern = c) 
                    let ps_total = 0.01f<ms/rate> + (ps |> List.sumBy (fun e -> e.Amount))
                    {
                        CorePattern = c
                        Mixed = true
                        BPM = None
                        Specific = None
                        Importance = importance ((ps |> List.sumBy (fun e -> e.Amount * e.Density50)) / ps_total) ps_total
                    }

                // todo: output something thats just a sum of mixed/non mixed
                // then output these that combine mixed+non mixed
                yield backup_for_core_pattern Stream
                yield backup_for_core_pattern Chordstream
                yield backup_for_core_pattern Jack
            }
            |> Seq.sortByDescending _.Importance
            |> List.ofSeq
        
        let top = List.head fragments
        let major : CategoryFragment list =
            fragments
            |> List.filter (fun f -> f.Importance / top.Importance > 0.5f && f.Importance / total > 0.2f)
        let minor = 
            fragments
            |> List.except major
            |> List.filter (fun f -> f.Importance / total > 0.1f)
        let is_not_duplicate_of_major (f: CategoryFragment) =
            f.Specific.IsSome
            || (
                major |> List.forall(fun x -> not (x.MoreUsefulThan f))
            )
        let is_not_duplicate (f: CategoryFragment) =
            f.Specific.IsSome
            || (
                major |> List.forall(fun x -> not (x.MoreUsefulThan f))
                && minor |> List.forall(fun x -> not (x.MoreUsefulThan f))
            )

        let major = major |> List.filter is_not_duplicate_of_major
        let minor = minor |> List.filter is_not_duplicate

        let minor_specific (spec: string) =
            minor |> List.tryFind (fun x -> x.Specific = Some spec)

        let notable_jacks (list: CategoryFragment list) (bpm: int option) =
            match bpm with
            | Some b ->
                let threshold = b / 2 + 5
                list |> List.exists (fun x -> x.CorePattern = Jack && (x.BPM.IsNone || x.BPM.Value > threshold))
            | None -> list |> List.exists (fun x -> x.CorePattern = Jack)

        let stream_name_prefix (f: CategoryFragment) =
            assert(f.CorePattern <> Jack)
            if f.CorePattern = Stream then "Stream"
            else
                match f.Specific with
                | Some "Jumpstream" -> 
                    match minor_specific "Handstream" with
                    | Some _ -> "Jumpstream/Handstream"
                    | _ -> "Jumpstream"
                | Some "Handstream" -> "Handstream"
                | _ ->
                    match minor_specific "Handstream" with
                    | Some _ -> "Jumpstream/Handstream"
                    | None -> 
                        if keys = 4 then 
                            "Jumpstream"
                        else 
                            "Chordstream"

        let category =
            match major with
            | [] -> "Uncategorised"
            | x :: [] ->
                match x.CorePattern with
                | Stream
                | Chordstream ->
                    let prefix = stream_name_prefix x
                    let is_hybrid = notable_jacks minor x.BPM
                    let hybrid_suffix = if is_hybrid then " Hybrid" else ""
                    let is_tech = x.Mixed
                    let tech_suffix = if is_tech then " Tech" else ""
                    prefix + hybrid_suffix + tech_suffix
                | Jack ->
                    match x.Specific with
                    | Some "Chordjacks" -> "Chordjack"
                    | _ -> "Jack"
            | _ ->
                let has_streams = major |> List.exists (fun x -> x.CorePattern <> Jack)
                let has_stream_tech = major |> List.exists (fun x -> x.CorePattern <> Jack && x.Mixed)
                let has_jacks = major |> List.exists (fun x -> x.CorePattern = Jack)
                let has_notable_jacks =
                    if has_jacks && has_streams then
                        if major.[0].CorePattern = Jack then true
                        else
                            let s = major |> List.tryFind (fun x -> x.CorePattern = Chordstream)
                            let j = major |> List.find (fun x -> x.CorePattern = Jack)
                            s.IsNone || s.Value.BPM.IsNone || j.BPM.IsNone || (5 + s.Value.BPM.Value / 2 < j.BPM.Value)
                    else false

                if has_streams && has_notable_jacks then
                    if has_stream_tech then "Hybrid Tech" else "Hybrid"

                elif has_streams then
                    let s = major |> List.find (fun x -> x.CorePattern <> Jack)
                    let prefix = stream_name_prefix s
                    let is_hybrid = notable_jacks minor s.BPM
                    let hybrid_suffix = if is_hybrid then " Hybrid" else ""
                    let tech_suffix = if has_stream_tech then " Tech" else ""
                    prefix + hybrid_suffix + tech_suffix

                else "Jack"

        {
            Category = category
            MajorFeatures = major |> List.map (fun x -> x.ToString())
            MinorFeatures = minor |> List.map (fun x -> x.ToString())
        }

    [<Json.AutoCodec>]
    type PatternDetailsReport =
        {
            Patterns: PatternBreakdown list
            LNPercent: float32
            SVAmount: Time
            Category: ChartCategorisation
        }
        static member Default = { Patterns = []; LNPercent = 0.0f; SVAmount = 0.0f<ms>; Category = ChartCategorisation.Default }

    let generate_detailed_pattern_data (rate: float32, chart: Chart) : PatternDetailsReport =
        let core_patterns, specific_patterns = Patterns.analyse rate chart

        let breakdown =
            core_patterns
            |> cluster_pattern_bpms
            |> Seq.filter (fun (_, info) -> info.BPM.Value >= 70)
            |> pattern_breakdown specific_patterns
            |> Seq.sortByDescending (fun x -> x.Amount)
            |> List.ofSeq
            |> List.truncate 16

        let is_useless (pattern: PatternBreakdown) =
            breakdown
            |> Seq.exists (fun p ->
                p.Pattern = pattern.Pattern
                && p.Amount * 0.5f > pattern.Amount
                && p.BPM > pattern.BPM
                && p.Mixed = pattern.Mixed
            )

        let patterns = breakdown |> List.filter (is_useless >> not)

        {
            Patterns = patterns
            LNPercent = ln_percent chart
            SVAmount = sv_time chart
            Category = categorise_chart chart.Keys patterns
        }

    let generate_cached_pattern_data (rate: float32, chart: Chart) : PatternDetailsReport =
        let core_patterns, specific_patterns = Patterns.analyse rate chart

        let breakdown =
            core_patterns
            |> cluster_pattern_bpms
            |> Seq.filter (fun (_, info) -> info.BPM.Value >= 70)
            |> pattern_breakdown specific_patterns
            |> Seq.sortByDescending (fun x -> x.Amount)
            |> List.ofSeq
            |> List.truncate 16

        let is_useless (pattern: PatternBreakdown) =
            breakdown
            |> Seq.exists (fun p ->
                p.Pattern = pattern.Pattern
                && p.Amount * 0.5f > pattern.Amount
                && p.BPM > pattern.BPM
                && p.Mixed = pattern.Mixed
            )

        let patterns = breakdown |> List.filter (is_useless >> not)

        {
            Patterns = patterns |> List.truncate 6
            LNPercent = ln_percent chart
            SVAmount = sv_time chart
            Category = categorise_chart chart.Keys patterns
        }
