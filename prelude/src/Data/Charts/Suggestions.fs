namespace Prelude.Data.Charts.Suggestions

open System
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Sorting
open Prelude.Data.Scores

type ChallengeLevel =
    | TooEasy
    | JustRight
    | TooHard

module Suggestion =

    let private estimate_challenge_level (rulesetId: string) (data: ChartSaveData) =
        match data.Bests.TryGetValue rulesetId with
        | true, d ->
            if fst d.Accuracy.Best > 0.965 then TooEasy
            elif fst d.Accuracy.Best < 0.945 then TooHard
            else JustRight
        | false, _ -> if (DateTime.Now - data.LastPlayed).TotalDays < 7.0 then TooHard else JustRight
    
    let get_suggestion (chart: Chart) (cacheData: CachedChart) (filter: Filter) (rulesetId: string) =

        let data = Scores.getOrCreateData chart
        let challenge_level = estimate_challenge_level rulesetId data

        let min_difficulty = cacheData.Physical - match challenge_level with TooHard -> 0.5 | JustRight -> 0.25 | TooEasy -> 0.1
        let max_difficulty = cacheData.Physical + match challenge_level with TooHard -> 0.1 | JustRight -> 0.25 | TooEasy -> 0.5

        let min_length : float = float (cacheData.Length / 1000.0f<ms>) - 60.0
        let max_length : float = min_length + 120.0

        let filter_and_constraints = 
            (Equals ("key", chart.Keys.ToString())) ::
            (LessThan ("diff", max_difficulty)) :: (MoreThan ("diff", min_difficulty)) :: 
            (LessThan ("length", max_length)) :: (MoreThan ("length", min_length)) ::
            filter

        let mutable best_chart = None
        let mutable best_chart_points = -1

        let bpm = 60000.0f<ms/minute> / fst cacheData.BPM
        let now = DateTime.Now
        let rand = Random()

        for chart in Filter.apply filter_and_constraints Library.charts.Values do
            
            // points out of 10
            let bpm_similarity : int = 10 - ( (bpm - 60000.0f<ms/minute> / fst chart.BPM) / 10.0f<beat/minute> |> MathF.Abs |> MathF.Truncate |> int )
            let recency_bonus : int =
                match Scores.getData chart.Hash with
                | Some d -> if d.LastPlayed = DateTime.UnixEpoch then 8 else Math.Clamp(Math.Log((now - d.LastPlayed).TotalDays, 1.4), 0, 10) |> int
                | None -> 8

            let score = 
                if chart.Hash = cacheData.Hash then -1 else 
                    bpm_similarity + recency_bonus * 2 + rand.Next(-3, 3)
            if score > best_chart_points then
                best_chart_points <- score
                best_chart <- Some chart

        best_chart