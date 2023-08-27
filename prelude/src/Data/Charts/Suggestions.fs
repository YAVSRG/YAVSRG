namespace Prelude.Data.Charts.Suggestions

open System
open Prelude
open Prelude.Charts.Formats.Interlude
open Prelude.Charts.Tools.Patterns
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
        | false, _ -> if (DateTime.UtcNow - data.LastPlayed).TotalDays < 7.0 then TooHard else JustRight

    let mutable recommended_recently = Set.empty

    let get_suggestions (chart: Chart) (cacheData: CachedChart) =
    
        recommended_recently <- Set.add cacheData.Hash recommended_recently
        let patterns = Patterns.generate_pattern_report (1.0f, chart)
        
        let min_difficulty = cacheData.Physical - 0.5
        let max_difficulty = cacheData.Physical + 0.5

        let min_length = cacheData.Length - 60000.0f<ms>
        let max_length = min_length + 120000.0f<ms>

        let now = DateTime.UtcNow

        let candidates =
            Library.cache.Entries.Values
            |> Seq.filter (fun x -> x.Keys = cacheData.Keys)
            |> Seq.filter (fun x -> x.Physical >= min_difficulty && x.Physical <= max_difficulty)
            |> Seq.filter (fun x -> x.Length >= min_length && x.Length <= max_length)
            |> Seq.filter (fun x -> match Scores.getData x.Hash with Some d -> (now - d.LastPlayed).TotalDays < 2 | _ -> true)
            |> Seq.filter (fun x -> Library.patterns.ContainsKey x.Hash)
            |> Seq.filter (fun x -> not (recommended_recently.Contains x.Hash))

        seq {
            for entry in candidates do
                let candidate_patterns = Library.patterns.[entry.Hash]

                let mutable similarity_score = 0.0f

                for p in patterns do
                    for p2 in candidate_patterns do
                        if p.Pattern = p2.Pattern then
                            let bpm_similarity = 
                                match p.Pattern with
                                | Stream _ -> 
                                    abs (p.BPM - p2.BPM) |> min 50 |> fun i -> 1.0f - float32 i / 50.0f
                                | Jack _ -> 
                                    abs (p.BPM - p2.BPM) |> min 25 |> fun i -> 1.0f - float32 i / 25.0f
                            let duration_similarity =
                                abs (p.Score - p.Score) |> float32 |> min 1000.0f |> fun i -> 1.0f - i / 1000.0f
                            similarity_score <- similarity_score + duration_similarity * bpm_similarity

                yield entry, similarity_score
        } |> Seq.sortByDescending snd |> Seq.map fst

    let get_suggestion (chart: Chart) (cacheData: CachedChart) =
        let rand = Random()
        
        let options = 
            get_suggestions chart cacheData
            |> Seq.truncate 100
            |> Array.ofSeq

        if options.Length = 0 then None else 
            
        let res = options.[rand.NextDouble() |> fun x -> x * x |> fun x -> x * float options.Length |> floor |> int]
        recommended_recently <- Set.add res.Hash recommended_recently
        Some res

    let get_random (filter: Filter) =
        let rand = Random()

        let charts =
            Filter.apply filter Library.cache.Entries.Values
            |> Array.ofSeq

        charts.[rand.Next charts.Length]
