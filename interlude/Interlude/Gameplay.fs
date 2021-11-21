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
open Prelude.Data.Scores
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

    let setScore (data: ScoreInfoProvider) : BestFlags =
        let d = chartSaveData.Value
        if
            // todo: score uploading goes here when implemented
            data.ModStatus < ModStatus.Unstored &&
            match Options.options.ScoreSaveCondition.Value with
            | _ -> true // todo: fill in this stub (pb condition will be complicated)
        then
            // add to score db
            d.Scores.Add data.ScoreInfo
            scores.Save()
            // update score buckets
            // update pbs
            if d.Bests.ContainsKey data.Scoring.Name then
                let existing = d.Bests.[data.Scoring.Name]
                let l, lp = PersonalBests.update (data.Lamp, data.ScoreInfo.rate) existing.Lamp
                let a, ap = PersonalBests.update (data.Scoring.Value, data.ScoreInfo.rate) existing.Accuracy
                let g, gp = PersonalBests.update (data.Grade, data.ScoreInfo.rate) existing.Grade
                let c, cp = PersonalBests.update (not data.HP.Failed, data.ScoreInfo.rate) existing.Clear
                d.Bests.[data.Scoring.Name] <-
                    {
                        Lamp = l
                        Accuracy = a
                        Grade = g
                        Clear = c
                    }
                { Lamp = lp; Accuracy = ap; Grade = gp; Clear = cp }
            else
                d.Bests.Add(
                    data.Scoring.Name,
                    {
                        Lamp = PersonalBests.create (data.Lamp, data.ScoreInfo.rate)
                        Accuracy = PersonalBests.create (data.Scoring.Value, data.ScoreInfo.rate)
                        Grade = PersonalBests.create (data.Grade, data.ScoreInfo.rate)
                        Clear = PersonalBests.create (not data.HP.Failed, data.ScoreInfo.rate)
                    }
                )
                { Lamp = PersonalBestType.Faster; Accuracy = PersonalBestType.Faster; Grade = PersonalBestType.Faster; Clear = PersonalBestType.Faster }
        else BestFlags.Default

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