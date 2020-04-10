namespace Prelude.Data

open System
open System.Collections.Generic
open Prelude.Charts.Interlude
open Prelude.Gameplay.Score
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Difficulty
open Prelude.Gameplay.Layout

module ScoreManager =

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
        Offset: float
        Scores: List<Score>
        Lamp: Dictionary<string, Lamp>
        Accuracy: Dictionary<string, float>
        Clear: Dictionary<string, bool>
    }

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

        member this.Save = ()

        static member Load =
            new Dictionary<string, ChartSaveData>()

    type TopScore = string * string * DateTime * float //Cache id, Hash, Timestamp, Rating

    type TopScores(scores: List<TopScore>) =
        new() = TopScores(new List<TopScore>())
        
        member this.AddScore((id, hash, timestamp, rating) : TopScore) =
            //running state through this fold:
                // 0 = Haven't found spot to insert this score at
                // 1 = Inserted this score
                // 2 = After insertion, removed old version of score from the list
            ()
            