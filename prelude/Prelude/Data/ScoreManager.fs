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

module ScoreManager =

    [<Json.AllRequired>]
    type Score = {
        time: DateTime
        hitdata: string
        rate: float32
        selectedMods: ModState
        layout: Layout
        keycount: int
    }
    //only present for performance reasons in Percyqaz.Json
    with static member Default = { time = DateTime.Now; hitdata = ""; rate = 1.0f; selectedMods = Map.empty; layout = Layout.Spread; keycount = 4 }

    type ChartSaveData = {
        [<Json.Required>]
        Offset: Setting<Time>
        [<Json.Required>]
        Scores: List<Score>
        Lamp: Dictionary<string, PersonalBests<Lamp>>
        Accuracy: Dictionary<string, PersonalBests<float>>
        Clear: Dictionary<string, PersonalBests<bool>>
    }
    with
        static member FromChart(c: Chart) = {
            Offset = Setting(c.Notes.First |> Option.map offsetOf |> Option.defaultValue 0.0f<ms>);
            Scores = List<Score>()
            Lamp = Dictionary<string, PersonalBests<Lamp>>()
            Accuracy = Dictionary<string, PersonalBests<float>>()
            Clear = Dictionary<string, PersonalBests<bool>>()
        }
        static member Default = { Offset = Setting(0.0f<ms>); Scores = null; Lamp = Dictionary<string, PersonalBests<Lamp>>(); Accuracy = Dictionary<string, PersonalBests<float>>(); Clear = Dictionary<string, PersonalBests<bool>>() }

    (*
        Gameplay pipelines that need to happen to play a chart
        Chart -> Modified chart -> Colorized chart
                                -> Replay data -> Mod replay data
                                -> Difficulty rating data
    *)

    type ScoreInfoProvider(score: Score, chart: Chart, scoring, hpSystem) =
        let mutable accuracyType = scoring
        let mutable hpType = hpSystem

        let mutable modchart = None
        let mutable modstring = None
        let mutable modstatus = None
        let mutable hitdata = None
        let mutable difficulty = None
        let mutable accMetric = None
        let mutable hpMetric = None
        let mutable perfMetric = None
        let mutable lamp = None

        member this.Score = score
        member this.Chart = chart

        member this.ModChart
            with get() =
                modchart <- Option.defaultWith (fun () -> getModChart score.selectedMods chart) modchart |> Some
                modchart.Value
            and set(value) = modchart <- Some value

        member this.HitData
            with get() =
                hitdata <- Option.defaultWith (fun () -> loadScoreData this.ModChart score.hitdata) hitdata |> Some
                hitdata.Value
            and set(value) = hitdata <- Some value

        member this.Difficulty
            with get() =
                difficulty <- Option.defaultWith (fun () -> RatingReport(this.ModChart.Notes, score.rate, score.layout, this.ModChart.Keys)) difficulty |> Some
                difficulty.Value
            and set(value) = difficulty <- Some value

        member this.AccuracyType with get() = accuracyType and set(value) = if value <> this.AccuracyType then accuracyType <- value; accMetric <- None
        member this.Accuracy =
            accMetric <- Option.defaultWith (fun () -> let m = createAccuracyMetric accuracyType in m.ProcessAll this.HitData; m) accMetric |> Some
            accMetric.Value

        member this.HPType with get() = hpType and set(value) = hpType <- value; hpMetric <- None
        member this.HP =
            hpMetric <- Option.defaultWith (fun () -> let m = createHPMetric hpType this.Accuracy in m.ProcessAll this.HitData; m) hpMetric |> Some
            hpMetric.Value

        member this.Performance =
            perfMetric <- Option.defaultWith (fun () -> let m = performanceMetric this.Difficulty this.ModChart.Keys in m.ProcessAll this.HitData; m) perfMetric |> Some
            perfMetric.Value

        member this.Lamp =
            lamp <- Option.defaultWith (fun () -> Prelude.Gameplay.Score.lamp this.Accuracy.State) lamp |> Some
            lamp.Value

        member this.Physical = this.Performance.Value
        member this.Technical = 0.0 //nyi
        member this.Mods =
            modstring <-
                Option.defaultWith
                    (fun () -> getModString(score.rate, score.selectedMods))
                    modstring |> Some
            modstring.Value

        member this.ModStatus =
            modstatus <-
                Option.defaultWith
                    (fun () -> score.selectedMods |> ModState.enumerate |> List.ofSeq |> List.map (fun s -> modList.[s].Status) |> fun l -> ModStatus.Ranked :: l |> List.max)
                    modstatus |> Some
            modstatus.Value

    type ScoresDB() =
        let data = ScoresDB.Load()

        member this.Save() = Json.toFile (Path.Combine(getDataPath "Data", "scores.json"), true) data
        static member Load() = loadImportantJsonFile "Scores" (Path.Combine(getDataPath "Data", "scores.json")) (new Dictionary<string, ChartSaveData>()) true

        member this.GetOrCreateScoreData(chart: Chart) =
            let hash = Chart.hash chart
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