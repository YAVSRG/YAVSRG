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
    | Variety
    //| SnipeScores
    | Consistency

type ChartSuggestionContext =
    {
        BaseDifficulty: float
        BaseChart: CachedChart * float32
        Priority: SuggestionPriority
        Filter: Filter
        Mods: ModState
        RulesetId: string
        Ruleset: Ruleset
        Library: Library
        ScoreDatabase: ScoreDatabase
    }
    member this.LibraryViewContext: LibraryViewContext =
        {
            Rate = let (cc, rate) = this.BaseChart in rate
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
                        1.0f - 10.0f * MathF.Abs(MathF.Log(float32 p.BPM / float32 p2.BPM)) |> max -1.0f
                    similarity <- similarity + bpm_similarity * (min p.Amount p2.Amount) / total_amount
        similarity   

    let private get_suggestions (ctx: ChartSuggestionContext) : (CachedChart * float32) seq =

        let base_chart, rate = ctx.BaseChart

        match Cache.patterns_by_hash base_chart.Hash ctx.Library.Cache with
        | None -> Seq.empty
        | Some patterns ->

        recommended_recently <- Set.add base_chart.Hash recommended_recently
        recommended_recently <- Set.add (base_chart.Title.ToLower()) recommended_recently

        let min_difficulty = base_chart.Physical * 0.9
        let max_difficulty = base_chart.Physical * 1.1

        let min_length = base_chart.Length - 60000.0f<ms>
        let max_length = min_length + 120000.0f<ms>

        let max_ln_pc = patterns.LNPercent + 0.1f
        let min_ln_pc = patterns.LNPercent - 0.1f

        let now = Timestamp.now ()
        let SEVEN_DAYS = 7L * 24L * 3600_000L

        let candidates =
            ctx.Library.Cache.Entries.Values
            |> Seq.filter (fun x -> x.Keys = base_chart.Keys)
            |> Seq.filter (fun x -> x.Physical >= min_difficulty && x.Physical <= max_difficulty)
            |> Seq.filter (fun x -> x.Length >= min_length && x.Length <= max_length)
            |> Seq.filter (fun x -> not (recommended_recently.Contains x.Hash))
            |> Seq.filter (fun x -> not (recommended_recently.Contains (x.Title.ToLower())))
            |> Seq.choose (fun x -> match Cache.patterns_by_hash x.Hash ctx.Library.Cache with Some p -> Some (x, p) | None -> None)
            |> Seq.filter (fun (x, p) -> p.LNPercent >= min_ln_pc && p.LNPercent <= max_ln_pc)
            |> Seq.filter (fun (x, p) -> now - (ScoreDatabase.get x.Hash ctx.ScoreDatabase).LastPlayed > SEVEN_DAYS)
            |> Filter.apply_ctx_seq (ctx.Filter, ctx.LibraryViewContext)

        let total_pattern_amount = patterns.Patterns |> Seq.sumBy (fun x -> x.Amount)

        seq {
            for entry, candidate_patterns in candidates do

                let mutable similarity_score = pattern_similarity total_pattern_amount patterns candidate_patterns

                if (patterns.SVAmount < 30000.0f<ms>) <> (candidate_patterns.SVAmount < 30000.0f<ms>) then
                    similarity_score <- similarity_score - 0.5f

                let difficulty_similarity =
                    abs (entry.Physical * float rate - ctx.BaseDifficulty) / ctx.BaseDifficulty * 5.0
                    |> min 1.0
                    |> fun x -> 1.0 - x * x
                    |> float32

                similarity_score <- similarity_score * difficulty_similarity

                yield entry, similarity_score
        }
        |> Seq.sortByDescending snd
        |> Seq.map (fun (cc, _) -> (cc, rate))

    let get_suggestion (ctx: ChartSuggestionContext) : (CachedChart * float32) option =
        let rand = Random()

        let best_matches = get_suggestions ctx |> Seq.truncate 50 |> Array.ofSeq

        if best_matches.Length = 0 then
            None
        else

            let cc, rate =
                let index =
                    rand.NextDouble()
                    |> fun x -> x * x
                    |> fun x -> x * float best_matches.Length
                    |> floor
                    |> int

                best_matches.[index]

            recommended_recently <- Set.add cc.Hash recommended_recently
            recommended_recently <- Set.add (cc.Title.ToLower()) recommended_recently

            Some (cc, rate)

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
            | Some (next_cc, rate) ->
                Some
                    {
                        Chart = next_cc
                        Rate = rate
                        Mods = ctx.Mods
                        NewState = 
                            EndlessModeState.Normal
                                { ctx with
                                    BaseChart =
                                        match ctx.Priority with
                                        | SuggestionPriority.Consistency -> ctx.BaseChart
                                        | SuggestionPriority.Variety -> next_cc, rate
                                    // base difficulty adjustment over time
                                }
                    }
            | None -> None