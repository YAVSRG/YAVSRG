namespace Prelude.Data

open System
open System.IO
open System.Collections.Generic
open Percyqaz.Json
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Gameplay.Score
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Difficulty
open Prelude.Gameplay.Layout
open Prelude.Data.ChartManager

module ScoreManager =

    //todo: add json required attributes to these records OR a way to insert default values
    type Score = {
        time: DateTime
        hitdata: string
        rate: float32
        selectedMods: ModState
        layout: Layout
        keycount: int
    }

    type ChartSaveData = {
        Offset: Setting<Time>
        Scores: List<Score>
        Lamp: Dictionary<string, PersonalBests<Lamp>>
        Accuracy: Dictionary<string, PersonalBests<float>>
        Clear: Dictionary<string, PersonalBests<bool>>
    }
    with
        static member FromChart(c: Chart) = {
            Offset = Setting(if c.Notes.IsEmpty() then 0.0f<ms> else offsetOf <| c.Notes.First());
            Scores = new List<Score>()
            Lamp = new Dictionary<string, PersonalBests<Lamp>>()
            Accuracy = Dictionary<string, PersonalBests<float>>()
            Clear = Dictionary<string, PersonalBests<bool>>()
        }

    (*
        Gameplay pipelines that need to happen to play a chart
        Chart -> Modified chart -> Colorized chart
                                -> Replay data -> Mod replay data
                                -> Difficulty rating data
    *)

    type ScoreInfoProvider(score: Score, chart: Chart) =
        //todo: method to set already existing values when possible
        let (modchart, hitdata) = getModChartWithScore (score.selectedMods) chart score.hitdata
        let difficulty =
            lazy (let (keys, notes, _, _, _) = modchart.Force()
                RatingReport(notes, score.rate, score.layout, keys))
        let accuracy =
            lazy (let m = createAccuracyMetric(SCPlus 4) in m.ProcessAll(hitdata.Force()); m) //todo: connect to profile settings
        let hp = lazy (let m = createHPMetric VG (accuracy.Force()) in m.ProcessAll(hitdata.Force()); m)
        let performance =
            lazy (
                let (keys, _, _, _, _) = modchart.Force()
                let m = performanceMetric (difficulty.Force()) keys in m.ProcessAll(hitdata.Force()); m)
        let lamp = lazy (lamp (accuracy.Force().State))

        member this.Score = score
        member this.ScoreData = hitdata.Force()
        member this.Accuracy = accuracy.Force()
        member this.HP = hp.Force()
        member this.Lamp = lamp.Force()
        member this.Physical = performance.Force().Value
        member this.Technical = 0.0 //nyi
        member this.Mods = String.Join(", ", sprintf "%.2fx" score.rate :: (score.selectedMods.Keys |> List.ofSeq |> List.map ModState.getModName))
        member this.Chart = chart

    type ScoresDB() =
        let data = ScoresDB.Load()

        //todo: automatic backups
        member this.Save() = Json.toFile((Path.Combine(getDataPath("Data"), "scores.json"), true)) data

        static member Load() =
            match Json.fromFile(Path.Combine(getDataPath("Data"), "scores.json")) with
            | Json.JsonParseResult.Success o -> o
            | Json.JsonParseResult.MappingFailure err
            | Json.JsonParseResult.ParsingFailure err -> Logging.Critical("Could not load score database! Creating from scratch") (err.ToString()); new Dictionary<string, ChartSaveData>()

        member this.GetOrCreateScoreData(chart: Chart) =
            let hash = calculateHash(chart)
            if hash |> data.ContainsKey |> not then data.Add(hash, ChartSaveData.FromChart(chart))
            data.[hash]

        member this.GetScoreData(hash: string) =
            if hash |> data.ContainsKey |> not then None else Some data.[hash]
            

    type TopScore = string * DateTime * float //Hash, Timestamp, Rating

    module TopScore =
        let private count = 50

        let add ((hash, timestamp, rating): TopScore) (data: TopScore list) =
            let rec f count data =
                match count with
                | 0 -> []
                | 1 ->
                    match data with
                    | (h, t, r) :: _ ->
                        if r >= rating then
                            (h, t, r) :: []
                        else
                            (hash, timestamp, rating) :: []
                    | [] -> []
                | _ ->
                    match data with
                    | (h, t, r) :: xs ->
                        if h = hash then
                            if r >= rating then
                                (h, t, r) :: xs
                            else
                                (hash, timestamp, rating) :: xs
                        else
                            if r >= rating then
                                (h, t, r) :: (f (count - 1) xs)
                            else
                                (hash, timestamp, rating) :: (h, t, r) :: (f (count - 2) xs)
                    | [] -> []
            f count data