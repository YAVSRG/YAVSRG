namespace Prelude.Charts.Processing.Patterns

open Percyqaz.Data
open Prelude
open Prelude.Charts


[<Json.AutoCodec>]
type ChartCategorisation =
    {
        Category: string
        MajorFeatures: string list
        MinorFeatures: string list
    }
    static member Default = { Category = "Unknown"; MajorFeatures = []; MinorFeatures = [] }

module Categorise =

    let SV_AMOUNT_THRESHOLD = 2000.0f<ms>

    // todo: make into a union for easier reasoning
    type private CategoryFragment =
        {
            Pattern: CorePatternType
            Mixed: bool // todo: turn 3-pronged with mixed, non mixed, combo of both
            BPM: int option
            Specific: string option
            Importance: GameplayTime
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
                    match this.Pattern with
                    | Stream -> "Streams"
                    | Chordstream -> "Chordstream"
                    | Jack -> "Jacks"
                | Some bpm ->
                    match this.Pattern with
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
            if f.Pattern <> this.Pattern then false else

            (f.BPM.IsNone && this.BPM.IsSome)
            || (f.Specific.IsNone && this.Specific.IsSome)

    let categorise_chart (keys: int) (patterns: PatternBreakdown list) (sv_amount: Time) : ChartCategorisation =

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
                            Pattern = p.Pattern
                            Mixed = p.Mixed
                            BPM = Some p.BPM
                            Specific = Some spec
                            Importance = importance p.Density50 spec_amount
                        }

                for p in patterns do
                    yield {
                        Pattern = p.Pattern
                        Mixed = p.Mixed
                        BPM = Some p.BPM
                        Specific = None
                        Importance = importance p.Density50 p.Amount
                    }

                let backup_for_core_pattern (c: CorePatternType) =
                    let ps = patterns |> List.filter (fun e -> e.Pattern = c) 
                    let ps_total = 0.01f<ms/rate> + (ps |> List.sumBy (fun e -> e.Amount))
                    {
                        Pattern = c
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
                list |> List.exists (fun x -> x.Pattern = Jack && (x.BPM.IsNone || x.BPM.Value > threshold))
            | None -> list |> List.exists (fun x -> x.Pattern = Jack)

        let stream_name_prefix (f: CategoryFragment) =
            assert(f.Pattern <> Jack)
            if f.Pattern = Stream then "Stream"
            else
                match f.Specific with
                | Some "Jumpstream" -> 
                    match minor_specific "Handstream" with
                    | Some x when x.BPM = f.BPM -> "Jumpstream/Handstream"
                    | _ -> "Jumpstream"
                | Some "Handstream" -> "Handstream"
                | _ ->
                    match minor_specific "Handstream" with
                    | Some x when x.BPM = f.BPM -> "Jumpstream/Handstream"
                    | _ -> 
                        if keys = 4 then 
                            "Jumpstream"
                        else 
                            "Chordstream"

        let category =
            match major with
            | [] -> "Uncategorised"
            | x :: [] ->
                match x.Pattern with
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
                    | _ ->
                        match minor_specific "Chordjacks" with
                        | Some cj when x.BPM = cj.BPM -> "Chordjack"
                        | _ -> "Jack"
            | _ ->
                let has_streams = major |> List.exists (fun x -> x.Pattern <> Jack)
                let has_stream_tech = major |> List.exists (fun x -> x.Pattern <> Jack && x.Mixed)
                let has_jacks = major |> List.exists (fun x -> x.Pattern = Jack)
                let has_notable_jacks =
                    if has_jacks && has_streams then
                        if major.[0].Pattern = Jack then true
                        else
                            let s = major |> List.tryFind (fun x -> x.Pattern = Chordstream)
                            let j = major |> List.find (fun x -> x.Pattern = Jack)
                            s.IsNone || s.Value.BPM.IsNone || j.BPM.IsNone || (5 + s.Value.BPM.Value / 2 < j.BPM.Value)
                    else false

                if has_streams && has_notable_jacks then
                    if has_stream_tech then "Hybrid Tech" else "Hybrid"

                elif has_streams then
                    let s = major |> List.find (fun x -> x.Pattern <> Jack)
                    let prefix = stream_name_prefix s
                    let is_hybrid = notable_jacks minor s.BPM
                    let hybrid_suffix = if is_hybrid then " Hybrid" else ""
                    let tech_suffix = if has_stream_tech then " Tech" else ""
                    prefix + hybrid_suffix + tech_suffix

                else
                    let j = major |> List.find (fun x -> x.Pattern = Jack)
                    match minor_specific "Chordjacks" with
                    | Some cj when cj.BPM = j.BPM -> "Chordjack"
                    | _ -> "Jack"

        {
            Category = category + if sv_amount > SV_AMOUNT_THRESHOLD then " + SV" else ""
            MajorFeatures = major |> List.map (fun x -> x.ToString())
            MinorFeatures = minor |> List.map (fun x -> x.ToString())
        }