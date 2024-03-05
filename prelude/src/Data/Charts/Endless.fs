namespace Prelude.Data.Charts.Endless

open System
open Prelude
open Prelude.Gameplay.Mods
open Prelude.Charts.Tools.Patterns
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Sorting
open Prelude.Data.Charts.Collections
open Prelude.Data.Scores

[<RequireQualifiedAccess>]
type VarietyLevel =
    | Low
    | High

[<RequireQualifiedAccess>]
type SuggestionMode =
    | Fun
    | Snipe
    | Challenge

type ChartSuggestionContext =
    {
        BaseChart: CachedChart
        Filter: Filter
        Rate: float32
        Mods: ModState
    }

module Suggestion =

    let mutable recommended_recently = Set.empty

    let get_similar_suggestions (ctx: ChartSuggestionContext) : CachedChart seq option =

        let cache_info = ctx.BaseChart

        recommended_recently <- Set.add cache_info.Hash recommended_recently

        if not (Library.patterns.ContainsKey cache_info.Hash) then
            None
        else

            let report = Library.patterns.[cache_info.Hash]
            let patterns = report.Patterns
            let total_pattern_amount = patterns |> Seq.sumBy (fun x -> x.Amount)

            let min_difficulty = cache_info.Physical - 0.5
            let max_difficulty = cache_info.Physical + 0.5

            let min_length = cache_info.Length - 60000.0f<ms>
            let max_length = min_length + 120000.0f<ms>

            let max_ln_pc = report.LNPercent + 0.1f
            let min_ln_pc = report.LNPercent - 0.1f

            let now = DateTime.UtcNow

            let candidates =
                Library.cache.Entries.Values
                |> Seq.filter (fun x -> x.Keys = cache_info.Keys)
                |> Seq.filter (fun x -> x.Physical >= min_difficulty && x.Physical <= max_difficulty)
                |> Seq.filter (fun x -> x.Length >= min_length && x.Length <= max_length)
                |> Seq.filter (fun x -> not (recommended_recently.Contains x.Hash))
                |> Seq.filter (fun x -> Library.patterns.ContainsKey x.Hash)
                |> Seq.filter (fun x ->
                    let ln_pc = Library.patterns.[x.Hash].LNPercent in ln_pc >= min_ln_pc && ln_pc <= max_ln_pc
                )
                |> Seq.filter (fun x ->
                    match Scores.get x.Hash with
                    | Some d -> (now - d.LastPlayed).TotalDays > 2.0
                    | _ -> true
                )

                |> Filter.apply ctx.Filter

            seq {
                for entry in candidates do
                    let candidate_patterns = Library.patterns.[entry.Hash]

                    let mutable similarity_score = 0.0f

                    if report.SVAmount > 30000.0f<ms> && candidate_patterns.SVAmount < 30000.0f<ms> then
                        similarity_score <- similarity_score - 1000.0f

                    if report.SVAmount < 30000.0f<ms> && candidate_patterns.SVAmount > report.SVAmount then
                        similarity_score <- similarity_score - 1000.0f

                    for p in patterns do
                        for p2 in candidate_patterns.Patterns do
                            if p.Pattern = p2.Pattern && p.Mixed = p2.Mixed then
                                let bpm_similarity =
                                    match p.Pattern with
                                    | Stream -> abs (p.BPM - p2.BPM) |> min 30 |> (fun i -> 1.0f - float32 i / 30.0f)
                                    | Chordstream -> abs (p.BPM - p2.BPM) |> min 30 |> (fun i -> 1.0f - float32 i / 30.0f)
                                    | Jack -> abs (p.BPM - p2.BPM) |> min 15 |> (fun i -> 1.0f - float32 i / 15.0f)

                                let duration_similarity =
                                    (min p.Amount p2.Amount) / total_pattern_amount

                                let bonus_similarity =
                                    if Array.tryHead p.Specifics = Array.tryHead p2.Specifics then 1.2f else 1.0f

                                similarity_score <- similarity_score + duration_similarity * bpm_similarity * bonus_similarity

                    if similarity_score > 0.5f then
                        yield entry, similarity_score
            }
            |> Seq.sortByDescending snd
            |> Seq.map fst
            |> Some

    let get_suggestion (ctx: ChartSuggestionContext) =
        let rand = Random()

        match get_similar_suggestions ctx with
        | Some matches ->
            let best_matches = matches |> Seq.truncate 100 |> Array.ofSeq

            if best_matches.Length = 0 then
                None
            else

                let res =
                    let index =
                        rand.NextDouble()
                        |> fun x -> x * x
                        |> fun x -> x * float best_matches.Length
                        |> floor
                        |> int

                    best_matches.[index]

                recommended_recently <- Set.add res.Hash recommended_recently
                Some res
        | None -> None

    let get_random (filter: Filter) : CachedChart option =
        let rand = Random()

        let charts = Filter.apply filter Library.cache.Entries.Values |> Array.ofSeq

        if charts.Length > 0 then
            Some charts.[rand.Next charts.Length]
        else
            None

[<RequireQualifiedAccess>]
type EndlessModeState =
    | Playlist of (CachedChart * PlaylistEntryInfo) list
    | Normal of ChartSuggestionContext

module EndlessModeState =

    let private shuffle_playlist_charts (items: 'T seq) =
        let random = new Random()
        items
        |> Seq.map (fun x -> x, random.Next())
        |> Seq.sortBy snd
        |> Seq.map fst
    
    let create_from_playlist (shuffle: bool) (playlist: Playlist) =
        playlist.Charts
        |> Seq.choose (fun (c, info) ->
            match Cache.by_hash c.Hash Library.cache with
            | Some cc -> Some (cc, info)
            | None -> None
        )
        |> if shuffle then shuffle_playlist_charts else id
        |> List.ofSeq
        |> EndlessModeState.Playlist

    let create (ctx: ChartSuggestionContext) = EndlessModeState.Normal ctx

    type Next =
        {
            Chart: CachedChart
            Rate: float32
            Mods: ModState
            NewState: EndlessModeState
        }

    let next (state: EndlessModeState) : Next option =
        match state with
        | EndlessModeState.Playlist [] -> None
        | EndlessModeState.Playlist ((chart, { Rate = rate; Mods = mods }) :: xs) ->
            Some {
                Chart = chart
                Rate = rate.Value
                Mods = mods.Value
                NewState = EndlessModeState.Playlist xs
            }
        | EndlessModeState.Normal ctx ->
            match Suggestion.get_suggestion ctx with
            | Some next_cc ->
                Some {
                    Chart = next_cc
                    Rate = ctx.Rate
                    Mods = ctx.Mods
                    NewState = EndlessModeState.Normal { ctx with BaseChart = next_cc }
                }
            | None -> None