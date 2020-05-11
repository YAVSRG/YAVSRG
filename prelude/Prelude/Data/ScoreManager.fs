namespace Prelude.Data

open System
open System.IO
open System.Collections.Generic
open Prelude.Json
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
        player: string
        playerUUID: string
        rate: float
        selectedMods: ModState
        layout: Layout
        keycount: int
    }

    type ChartSaveData = {
        Path: string
        Offset: Setting<float>
        Scores: List<Score>
        Lamp: Dictionary<string, Lamp>
        Accuracy: Dictionary<string, float>
        Clear: Dictionary<string, bool>
    }
    with
        static member FromChart(c: Chart) =
            {   Path = c.FileIdentifier
                Offset = Setting(if c.Notes.IsEmpty then 0.0 else offsetOf c.Notes.First);
                Scores = new List<Score>()
                Lamp = new Dictionary<string, Lamp>()
                Accuracy = Dictionary<string, float>()
                Clear = Dictionary<string, bool>() }

    (*
        Gameplay pipelines that need to happen to play a chart
        Chart -> Modified chart -> Colorized chart
                                -> Replay data -> Mod replay data
                                -> Difficulty rating data
    *)

    type ScoreInfoProvider(score: Score, chart: Chart) =
        let (modchart, hitdata) = getModChartWithScore score.selectedMods chart score.hitdata
        let difficulty =
            lazy (let (keys, notes, _, _, _) = modchart.Force()
                RatingReport(notes, score.rate, score.layout, keys))
        let scoring =
            lazy (let m = createAccuracyMetric(SCPlus 4) in m.ProcessAll(hitdata.Force()); m) //todo: connect to profile settings
        let hp = lazy (let m = createHPMetric VG (scoring.Force()) in m.ProcessAll(hitdata.Force()); m)
        let performance =
            lazy (
                let (keys, _, _, _, _) = modchart.Force()
                let m = performanceMetric (difficulty.Force()) keys in m.ProcessAll(hitdata.Force()); m)
        let lamp = lazy (lamp (scoring.Force().State))

        member this.Accuracy = scoring.Force().Value
        member this.Clear = hp.Force().Failed
        member this.Lamp = lamp.Force()
        member this.Physical = performance.Force().Value
        member this.Technical = 0.0 //nyi
        member this.Mods = let (keys, _, _, _, mods) = modchart.Force() in String.Join(", ", mods)

    type ScoresDB() =
        let data = ScoresDB.Load

        member this.Save() = JsonHelper.saveFile data (Path.Combine(getDataPath("Data"), "scores.json"))

        static member Load =
            try
                JsonHelper.loadFile(Path.Combine(getDataPath("Data"), "scores.json"))
            with
            | :? FileNotFoundException -> Logging.Info("No scores database found, creating one.") ""; new Dictionary<string, ChartSaveData>()
            | err -> Logging.Critical("Could not load cache file! Creating from scratch") (err.ToString()); new Dictionary<string, ChartSaveData>()

        member this.GetScoreData (chart: Chart) =
            let hash = calculateHash(chart)
            if not <| data.ContainsKey(hash) then data.Add(hash, ChartSaveData.FromChart(chart))
            data.[hash]
            

    type TopScore = string * string * DateTime * float //Cache id, Hash, Timestamp, Rating

    type TopScores(scores: List<TopScore>) =
        new() = TopScores(new List<TopScore>())
        
        member this.AddScore((id, hash, timestamp, rating) : TopScore) =
            //running state through this fold:
                // 0 = Haven't found spot to insert this score at
                // 1 = Inserted this score
                // 2 = After insertion, removed old version of score from the list
            ()
            