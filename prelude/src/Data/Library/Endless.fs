namespace Prelude.Data.Library.Endless

open System
open Percyqaz.Common
open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.Mods
open Prelude.Charts.Processing.Patterns
open Prelude.Data.Library
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Sorting
open Prelude.Data.Library.Collections
open Prelude.Data

[<RequireQualifiedAccess>]
type SuggestionPriority =
    | Normal
    //| SnipeScores
    | LowVariety

type ChartSuggestionContext =
    {
        BaseChart: CachedChart
        Filter: Filter
        Mods: ModState
        Rate: float32
        RulesetId: string
        Ruleset: Ruleset
        Library: Library
        ScoreDatabase: ScoreDatabase
    }
    member this.LibraryViewContext: LibraryViewContext =
        {
            Rate = this.Rate
            RulesetId = this.RulesetId
            Ruleset = this.Ruleset
            Library = this.Library
            ScoreDatabase = this.ScoreDatabase
        }

module Suggestion =

    let mutable recommended_recently = Set.empty

    let private pattern_similarity (total_amount: ScaledTime) (base_patterns: PatternInfo) (other_patterns: PatternInfo) : float32 =
        let mutable similarity = 0.0f
        for p in base_patterns.Patterns do
            for p2 in other_patterns.Patterns do
                if p.Pattern = p2.Pattern && p.Mixed = p2.Mixed then
                    let bpm_similarity =
                        1.0f - 10.0f * MathF.Abs(MathF.Log(float32 p.BPM / float32 p2.BPM)) |> max 0.0f
                    similarity <- similarity + bpm_similarity * (min p.Amount p2.Amount) / total_amount
        similarity
                    

    let get_similar_suggestions (ctx: ChartSuggestionContext) : CachedChart seq option =

        let base_chart = ctx.BaseChart
        recommended_recently <- Set.add base_chart.Hash recommended_recently

        match Cache.patterns_by_hash base_chart.Hash ctx.Library.Cache with
        | None -> None
        | Some patterns ->

            let total_pattern_amount = patterns.Patterns |> Seq.sumBy (fun x -> x.Amount)

            let min_difficulty = base_chart.Physical - 0.5
            let max_difficulty = base_chart.Physical + 0.5

            let min_length = base_chart.Length - 60000.0f<ms>
            let max_length = min_length + 120000.0f<ms>

            let max_ln_pc = patterns.LNPercent + 0.1f
            let min_ln_pc = patterns.LNPercent - 0.1f

            let now = Timestamp.now ()
            let TWO_DAYS = 2L * 24L * 3600_000L

            let candidates =
                ctx.Library.Cache.Entries.Values
                |> Seq.filter (fun x -> x.Keys = base_chart.Keys)
                |> Seq.filter (fun x -> x.Physical >= min_difficulty && x.Physical <= max_difficulty)
                |> Seq.filter (fun x -> x.Length >= min_length && x.Length <= max_length)
                |> Seq.filter (fun x -> not (recommended_recently.Contains x.Hash))
                |> Seq.choose (fun x -> match Cache.patterns_by_hash x.Hash ctx.Library.Cache with Some p -> Some (x, p) | None -> None)
                |> Seq.filter (fun (x, p) -> p.LNPercent >= min_ln_pc && p.LNPercent <= max_ln_pc)
                |> Seq.filter (fun (x, p) -> now - (ScoreDatabase.get x.Hash ctx.ScoreDatabase).LastPlayed > TWO_DAYS)
                |> Filter.apply_ctx_seq (ctx.Filter, ctx.LibraryViewContext)

            seq {
                for entry, candidate_patterns in candidates do

                    let mutable similarity_score = pattern_similarity total_pattern_amount patterns candidate_patterns

                    if patterns.SVAmount > 30000.0f<ms> && candidate_patterns.SVAmount < 30000.0f<ms> then
                        similarity_score <- similarity_score - 0.5f

                    if patterns.SVAmount < 30000.0f<ms> && candidate_patterns.SVAmount > patterns.SVAmount then
                        similarity_score <- similarity_score - 0.5f

                    if similarity_score > 0.2f then
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

    let get_random (filter_by: Filter) (ctx: LibraryViewContext) : CachedChart option =
        let rand = Random()

        let charts =
            Filter.apply_seq (filter_by, ctx) ctx.Library.Cache.Entries.Values
            |> Array.ofSeq

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
        items |> Seq.map (fun x -> x, random.Next()) |> Seq.sortBy snd |> Seq.map fst

    let create_from_playlist (from: int) (playlist: Playlist) (library: Library) =
        playlist.Charts
        |> Seq.skip from
        |> Seq.choose (fun (c, info) ->
            match Cache.by_hash c.Hash library.Cache with
            | Some cc -> Some(cc, info)
            | None -> None
        )
        |> List.ofSeq
        |> EndlessModeState.Playlist

    let create_from_playlist_shuffled (playlist: Playlist) (library: Library) =
        playlist.Charts
        |> Seq.choose (fun (c, info) ->
            match Cache.by_hash c.Hash library.Cache with
            | Some cc -> Some(cc, info)
            | None -> None
        )
        |>  shuffle_playlist_charts
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
        | EndlessModeState.Playlist((chart, { Rate = rate; Mods = mods }) :: xs) ->
            Some
                {
                    Chart = chart
                    Rate = rate.Value
                    Mods = mods.Value
                    NewState = EndlessModeState.Playlist xs
                }
        | EndlessModeState.Normal ctx ->
            match Suggestion.get_suggestion ctx with
            | Some next_cc ->
                Some
                    {
                        Chart = next_cc
                        Rate = ctx.Rate
                        Mods = ctx.Mods
                        NewState = EndlessModeState.Normal { ctx with BaseChart = next_cc }
                    }
            | None -> None
