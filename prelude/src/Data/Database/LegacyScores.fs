namespace Prelude.Data.Scores

open System
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open Percyqaz.Data
open Percyqaz.Common
open Prelude
open Prelude.Charts.Processing.Difficulty
open Prelude.Gameplay
open Prelude.Gameplay.Mods

[<Json.AutoCodec(true)>]
type internal LegacyScore =
    {
        time: DateTime
        replay: string
        rate: float32
        selectedMods: ModState
        layout: Layout
        keycount: int
    }
    static member Default = { time = Unchecked.defaultof<_>; replay = ""; rate = 1.0f; selectedMods = Map.empty; layout = Layout.Spread; keycount = 4 }

[<Json.AutoCodec(true)>]
type LegacyBests =
    {
        Lamp: PersonalBests<int>
        Accuracy: PersonalBests<float>
        Grade: PersonalBests<int>
    }
    static member Default = { Lamp = []; Accuracy = []; Grade = [] }

[<Json.AutoCodec(false)>]
type internal LegacyChartSaveData =
    {
        [<Json.Required>]
        mutable Offset: Time
        [<Json.Required>]
        Scores: List<LegacyScore>
        PersonalBests: Dictionary<string, LegacyBests>
        mutable LastPlayed: DateTime
        mutable Comment: string
    }
    static member Default = { Offset = 0.0f<ms>; Scores = List<LegacyScore>(); PersonalBests = Dictionary<string, LegacyBests>(); LastPlayed = DateTime.UnixEpoch; Comment = "" }

[<Json.AutoCodec(false)>]
type LegacyScoreDatabase =
    internal {
        Entries: ConcurrentDictionary<string, LegacyChartSaveData>
    }
    static member Default =
        {
            Entries = new ConcurrentDictionary<string, LegacyChartSaveData>()
        }
    static member Load () : LegacyScoreDatabase =
        load_important_json_file "Scores" (Path.Combine(get_game_folder "Data", "scores.json")) true
        |> fun d ->
            Logging.Info(sprintf "Loaded legacy scores for %i charts." d.Entries.Keys.Count)
            d

//module Scores =

    //let deinit () = save()

    //let get_or_create (chart: Chart) =
    //    let hash = Chart.hash chart

    //    if not (data.Entries.ContainsKey hash) then
    //        data.Entries.[hash] <- ChartSaveData.FromChart chart

    //    data.Entries.[hash]

    //let get (hash: string) =
    //    if hash |> data.Entries.ContainsKey |> not then
    //        None
    //    else
    //        Some data.Entries.[hash]

    //let save_score (d: ChartSaveData) (score_info: ScoreInfo) =
    //    d.Scores.Add (ScoreInfo.to_score score_info)
    //    save ()

    //let save_score_pbs (d: ChartSaveData) (ruleset_id: string) (score_info: ScoreInfo) : ImprovementFlags =
    //    save_score d score_info

    //    if d.PersonalBests.ContainsKey ruleset_id then
    //        let newBests, flags = Bests.update score_info d.PersonalBests.[ruleset_id]
    //        d.PersonalBests.[ruleset_id] <- newBests
    //        flags
    //    else
    //        d.PersonalBests.Add(ruleset_id, Bests.create score_info)

    //        {
    //            Lamp = Improvement.New
    //            Accuracy = Improvement.New
    //            Grade = Improvement.New
    //        }
    
    //let get_best_grade_above (ruleset_id: string) (rate: float32) (chart_hash: string) : int option =
    //    match get chart_hash with
    //    | Some d ->
    //        if d.PersonalBests.ContainsKey ruleset_id then
    //            PersonalBests.get_best_above rate d.PersonalBests.[ruleset_id].Grade
    //        else
    //            None
    //    | None -> None