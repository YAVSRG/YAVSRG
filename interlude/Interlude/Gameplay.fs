namespace Interlude

open System
open System.Collections.Generic
open Prelude.Common
open Prelude.ChartFormats.Interlude
open Prelude.Gameplay.Mods
open Prelude.Scoring
open Prelude.Gameplay.Difficulty
open Prelude.Gameplay.NoteColors
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Data.ScoreManager
open Interlude
open Interlude.UI
open Interlude.Utils

module Gameplay =

    let mutable internal currentChart: Chart option = None
    let mutable internal currentCachedChart: CachedChart option = None
    let mutable internal chartSaveData = None
    let mutable modifiedChart: ModChart option = None
    let mutable private coloredChart: ColorizedChart option = None
    let mutable difficultyRating: RatingReport option = None

    let mutable rate = 1.0f
    let mutable selectedMods = Map.empty
    let mutable autoplay = false
    let scores = ScoresDB()

    let mutable onChartUpdate = ignore
    let mutable onChartChange = ignore

    let updateChart() =
        match currentChart with
        | None -> ()
        | Some c ->
            modifiedChart <- Some <| getModChart selectedMods c
            coloredChart <- None
            difficultyRating <-
                let mc = modifiedChart.Value in
                Some <| RatingReport(mc.Notes, rate, Options.options.Playstyles.[mc.Keys - 3], mc.Keys)
            onChartUpdate()

    let changeRate amount =
        rate <- Math.Round(float (rate + amount), 2) |> float32
        Audio.changeRate rate
        updateChart()

    let changeChart (cachedChart, chart) =
        currentCachedChart <- Some cachedChart
        currentChart <- Some chart
        chartSaveData <- Some <| scores.GetOrCreateScoreData chart
        Screen.Background.load chart.BackgroundPath
        Audio.changeTrack (chart.AudioPath, chartSaveData.Value.Offset - chart.FirstNote, rate)
        Audio.playFrom chart.Header.PreviewTime
        Options.options.CurrentChart.Value <- cachedChart.FilePath
        updateChart()
        onChartChange()

    let getColoredChart() =
        match modifiedChart with
        | None -> failwith "Tried to get coloredChart when no modifiedChart exists"
        | Some mc ->
            coloredChart <- Option.defaultWith (fun () -> getColoredChart (Content.noteskinConfig().NoteColors) mc) coloredChart |> Some
            coloredChart.Value

    let recolorChart() = coloredChart <- None

    let makeScore (replayData, keys) : Score = {
        time = DateTime.Now
        replay = Replay.compress replayData
        rate = rate
        selectedMods = selectedMods |> ModChart.filter modifiedChart.Value
        layout = Options.options.Playstyles.[keys - 3]
        keycount = keys
    }

    let setScore (data: ScoreInfoProvider) =
        let d = chartSaveData.Value
        if
            //todo: score uploading goes here when implemented
            data.ModStatus < ModStatus.Unstored &&
            match Options.options.ScoreSaveCondition.Value with
            | _ -> true //todo: fill in this stub (pb condition will be complicated)
        then
            //add to score db
            d.Scores.Add data.ScoreInfo
            scores.Save()
            //update top scores
            //Options.options.Stats.TopPhysical |> Setting.app (TopScore.add(currentCachedChart.Value.Hash, data.ScoreInfo.time, data.Physical))
            //Options.options.Stats.TopTechnical |> Setting.app (TopScore.add(currentCachedChart.Value.Hash, data.ScoreInfo.time, data.Technical))
            //update pbs
            let f name (target: Dictionary<string, PersonalBests<'T>>) (value: 'T) =
                if target.ContainsKey(name) then
                    let n, pb = PersonalBests.update (value, data.ScoreInfo.rate) target.[name]
                    target.[name] <- n
                    pb
                else
                    target.Add(name, ((value, data.ScoreInfo.rate), (value, data.ScoreInfo.rate)))
                    PersonalBestType.Faster
            f data.Scoring.Name d.Lamp data.Lamp,
            f data.Scoring.Name d.Accuracy data.Scoring.Value,
            //todo: maybe move this implentation to one place since it is doubled up in ScreenLevelSelect.cs
            f (data.Scoring.Name + "|" + data.HP.Name) d.Clear (not data.HP.Failed)
        else (PersonalBestType.None, PersonalBestType.None, PersonalBestType.None)

    let save() =
        scores.Save()
        Library.save()

    let init() =
        try
            let c, ch =
                match Library.lookup Options.options.CurrentChart.Value with
                | Some cc ->
                    match Library.load cc with
                    | Some c -> cc, c
                    | None ->
                        Logging.Error("Could not load chart file: " + cc.FilePath)
                        Library.getGroups (K "All") (Comparison(fun _ _ -> 0)) []
                        |> fun d -> d.["All"].[0]
                        |> fun c -> c, Library.load(c).Value
                | None ->
                    Logging.Info("Could not find cached chart: " + Options.options.CurrentChart.Value)
                    Library.getGroups(K "All") (Comparison(fun _ _ -> 0)) []
                    |> fun d -> d.["All"].[0]
                    |> fun c -> c, Library.load(c).Value
            changeChart(c, ch)
        with err ->
            Logging.Debug("Tried to auto select a chart but none exist", err)
            Screen.Background.load ""