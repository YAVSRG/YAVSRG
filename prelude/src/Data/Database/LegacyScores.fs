namespace Prelude.Data.Scores

open System
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open Percyqaz.Data
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Gameplay

[<Json.AutoCodec(false)>]
type ChartSaveData =
    {
        [<Json.Required>]
        mutable Offset: Time
        [<Json.Required>]
        Scores: List<Score>
        PersonalBests: Dictionary<string, Bests>
        mutable LastPlayed: DateTime
        mutable Comment: string
    }
    static member Default = { Offset = 0.0f<ms>; Scores = List<Score>(); PersonalBests = Dictionary<string, Bests>(); LastPlayed = DateTime.UnixEpoch; Comment = "" }
    static member FromChart(c: Chart) =
        {
            Offset = c.FirstNote
            Scores = List<Score>()
            PersonalBests = Dictionary<string, Bests>()
            LastPlayed = DateTime.UnixEpoch
            Comment = ""
        }

[<Json.AutoCodec(false)>]
type LegacyScoreDatabase =
    {
        Entries: ConcurrentDictionary<string, ChartSaveData>
    }
    static member Default =
        {
            Entries = new ConcurrentDictionary<string, ChartSaveData>()
        }

module Scores =

    // todo: move this to an SQLite database
    let mutable data : LegacyScoreDatabase = Unchecked.defaultof<_>

    let save () = save_important_json_file (Path.Combine(get_game_folder "Data", "scores.json")) data

    let init_startup() =
        data <-
            load_important_json_file "Scores" (Path.Combine(get_game_folder "Data", "scores.json")) true
            |> fun d ->
                Logging.Info(sprintf "Loaded scores for %i charts." d.Entries.Keys.Count)
                d

    let deinit () = save()

    let get_or_create (chart: Chart) =
        let hash = Chart.hash chart

        if not (data.Entries.ContainsKey hash) then
            data.Entries.[hash] <- ChartSaveData.FromChart chart

        data.Entries.[hash]

    let get (hash: string) =
        if hash |> data.Entries.ContainsKey |> not then
            None
        else
            Some data.Entries.[hash]

    let save_score (d: ChartSaveData) (score_info: ScoreInfo) =
        d.Scores.Add (ScoreInfo.to_score score_info)
        save ()

    let save_score_pbs (d: ChartSaveData) (ruleset_id: string) (score_info: ScoreInfo) : ImprovementFlags =
        save_score d score_info

        if d.PersonalBests.ContainsKey ruleset_id then
            let newBests, flags = Bests.update score_info d.PersonalBests.[ruleset_id]
            d.PersonalBests.[ruleset_id] <- newBests
            flags
        else
            d.PersonalBests.Add(ruleset_id, Bests.create score_info)

            {
                Lamp = Improvement.New
                Accuracy = Improvement.New
                Grade = Improvement.New
            }
    
    let get_best_grade_above (ruleset_id: string) (rate: float32) (chart_hash: string) =
        match get chart_hash with
        | Some d ->
            if d.PersonalBests.ContainsKey ruleset_id then
                PersonalBests.get_best_above rate d.PersonalBests.[ruleset_id].Grade
            else
                None
        | None -> None