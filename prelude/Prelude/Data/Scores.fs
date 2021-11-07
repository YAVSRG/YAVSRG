namespace Prelude.Data

open System
open System.IO
open System.Collections.Generic
open Percyqaz.Json
open Prelude.Common
open Prelude.ChartFormats.Interlude
open Prelude.Scoring
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Difficulty
open Prelude.Gameplay.Layout

module ScoreManager =

    [<Json.AllRequired>]
    type Score =
        {
            time: DateTime
            replay: string
            rate: float32
            selectedMods: ModState
            layout: Layout
            keycount: int
        }
    //only present for performance reasons in Percyqaz.Json
        static member Default =
            { 
                time = DateTime.Now
                replay = ""
                rate = 1.0f
                selectedMods = Map.empty
                layout = Layout.Spread; keycount = 4
            }

    type ChartSaveData =
        {
            [<Json.Required>]
            mutable Offset: Time
            [<Json.Required>]
            Scores: List<Score>
            Lamp: Dictionary<string, PersonalBests<Lamp>>
            Accuracy: Dictionary<string, PersonalBests<float>>
            Clear: Dictionary<string, PersonalBests<bool>>
        }
        static member FromChart(c: Chart) =
            {
                Offset = c.FirstNote
                Scores = List<Score>()
                Lamp = Dictionary<string, PersonalBests<Lamp>>()
                Accuracy = Dictionary<string, PersonalBests<float>>()
                Clear = Dictionary<string, PersonalBests<bool>>()
            }
        static member Default =
            {
                Offset = 0.0f<ms>
                Scores = null
                Lamp = Dictionary<string, PersonalBests<Lamp>>()
                Accuracy = Dictionary<string, PersonalBests<float>>()
                Clear = Dictionary<string, PersonalBests<bool>>()
            }

    (*
        Gameplay pipelines that need to happen to play a chart
        Chart -> Modified chart -> Colorized chart
                                -> Replay data -> Mod replay data
                                -> Difficulty rating data
    *)

    type ScoreInfoProvider(score: Score, chart: Chart, scoring, hpSystem) =
        let mutable accuracyType: Metrics.AccuracySystemConfig = scoring
        let mutable hpType: Metrics.HPSystemConfig = hpSystem

        let mutable modchart = ValueNone
        let mutable modstring = ValueNone
        let mutable modstatus = ValueNone
        let mutable difficulty = ValueNone
        let mutable scoreMetric = ValueNone
        let mutable replayData = ValueNone
        let mutable perf = ValueNone
        let mutable lamp = ValueNone

        member this.ScoreInfo = score
        member this.Chart = chart

        member this.ReplayData =
            replayData <- ValueOption.defaultWith (fun () -> Replay.decompress score.replay) replayData |> ValueSome
            replayData.Value

        member this.ModChart
            with get() =
                modchart <- ValueOption.defaultWith (fun () -> getModChart score.selectedMods chart) modchart |> ValueSome
                modchart.Value
            and set(value) = modchart <- ValueSome value

        member this.AccuracyType with get() = accuracyType and set(value) = if value <> this.AccuracyType then accuracyType <- value; scoreMetric <- ValueNone
        member this.Scoring =
            scoreMetric <-
                ValueOption.defaultWith (fun () -> 
                    let m = Metrics.createScoreMetric accuracyType hpType this.ModChart.Keys (StoredReplayProvider this.ReplayData) this.ModChart.Notes score.rate 
                    m.Update Time.infinity; m)
                    scoreMetric
                |> ValueSome
            scoreMetric.Value

        member this.HPType with get() = hpType and set(value) = hpType <- value; scoreMetric <- ValueNone
        member this.HP = this.Scoring.HP

        member this.Difficulty
            with get() =
                difficulty <- ValueOption.defaultWith (fun () -> RatingReport (this.ModChart.Notes, score.rate, score.layout, this.ModChart.Keys)) difficulty |> ValueSome
                difficulty.Value
            and set(value) = difficulty <- ValueSome value

        member this.Lamp =
            lamp <- ValueOption.defaultWith (fun () -> Prelude.Scoring.Lamp.calculate this.Scoring.State) lamp |> ValueSome
            lamp.Value

        // todo: grade info

        member this.Physical =
            perf <- ValueOption.defaultWith (fun () -> getRatings this.Difficulty score.keycount this.Scoring) perf |> ValueSome
            fst perf.Value
        member this.Technical =
            perf <- ValueOption.defaultWith (fun () -> getRatings this.Difficulty score.keycount this.Scoring) perf |> ValueSome
            snd perf.Value

        member this.Mods =
            modstring <-
                ValueOption.defaultWith
                    (fun () -> getModString(score.rate, score.selectedMods))
                    modstring |> ValueSome
            modstring.Value

        member this.ModStatus =
            modstatus <-
                ValueOption.defaultWith
                    (fun () -> score.selectedMods |> ModState.enumerate |> List.ofSeq |> List.map (fun s -> modList.[s].Status) |> fun l -> ModStatus.Ranked :: l |> List.max)
                    modstatus |> ValueSome
            modstatus.Value

    type ScoresDB() =
        let data = ScoresDB.Load()

        member this.Save() = JSON.ToFile (Path.Combine (getDataPath "Data", "scores.json"), true) data
        static member Load() = loadImportantJsonFile "Scores" (Path.Combine (getDataPath "Data", "scores.json")) (new Dictionary<string, ChartSaveData>()) true

        member this.GetOrCreateScoreData (chart: Chart) =
            let hash = Chart.hash chart
            if hash |> data.ContainsKey |> not then data.Add(hash, ChartSaveData.FromChart(chart))
            data.[hash]

        member this.GetScoreData (hash: string) =
            if hash |> data.ContainsKey |> not then None else Some data.[hash]


    // to be extended into a score buckety system soon
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
                    | [] -> (hash, timestamp, rating) :: []
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
                    | [] -> (hash, timestamp, rating) :: []
            f count data