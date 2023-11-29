namespace Prelude.Data.Charts.Suggestions

open System
open Prelude
open Prelude.Charts
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

    let mutable recommended_recently = Set.empty



    let get_similar_suggestions (cache_info: CachedChart) (filter: Filter) : CachedChart seq option =

        recommended_recently <- Set.add cache_info.Hash recommended_recently

        if not (Library.patterns.ContainsKey cache_info.Hash) then None else

        let report = Library.patterns.[cache_info.Hash]
        let patterns = report.Patterns

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
            |> Seq.filter (fun x -> let ln_pc = Library.patterns.[x.Hash].LNPercent in ln_pc >= min_ln_pc && ln_pc <= max_ln_pc)
            |> Seq.filter (fun x ->
                match Scores.get x.Hash with
                | Some d -> (now - d.LastPlayed).TotalDays > 2.0
                | _ -> true
            )

            |> Filter.apply filter

        seq {
            for entry in candidates do
                let candidate_patterns = Library.patterns.[entry.Hash]

                let mutable similarity_score = 0.0f

                if report.SVAmount > 30000.0f<ms> && candidate_patterns.SVAmount < 30000.0f<ms> then similarity_score <- similarity_score - 1000.0f
                if report.SVAmount < 30000.0f<ms> && candidate_patterns.SVAmount > report.SVAmount then similarity_score <- similarity_score - 1000.0f

                for p in patterns do
                    for p2 in candidate_patterns.Patterns do
                        if p.Pattern = p2.Pattern then
                            let bpm_similarity =
                                match p.Pattern with
                                | Stream _ -> abs (p.BPM - p2.BPM) |> min 50 |> (fun i -> 1.0f - float32 i / 50.0f)
                                | Jack _ -> abs (p.BPM - p2.BPM) |> min 25 |> (fun i -> 1.0f - float32 i / 25.0f)

                            let duration_similarity =
                                abs (p.Score - p2.Score)
                                |> float32
                                |> min 1000.0f
                                |> fun i -> 1.0f - i / 1000.0f

                            similarity_score <- similarity_score + duration_similarity * bpm_similarity

                yield entry, similarity_score
        }
        |> Seq.sortByDescending snd
        |> Seq.map fst
        |> Some

    let get_suggestion (cache_info: CachedChart) (filter: Filter) =
        let rand = Random()

        match get_similar_suggestions cache_info filter with
        | Some matches ->
            let best_matches = matches |> Seq.truncate 100 |> Array.ofSeq
            if best_matches.Length = 0 then None else

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

        if charts.Length > 0 then Some charts.[rand.Next charts.Length] else None
