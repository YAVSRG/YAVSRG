namespace Interlude

open System
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
open Interlude.Options
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
                Some <| RatingReport(mc.Notes, rate, options.Playstyles.[mc.Keys - 3], mc.Keys)
            onChartUpdate()

    let changeRate amount =
        rate <- Math.Round(float (rate + amount), 2) |> float32
        Audio.changeRate rate
        updateChart()

    let changeChart (cachedChart, chart) =
        currentCachedChart <- Some cachedChart
        currentChart <- Some chart
        chartSaveData <- Some <| Scores.getOrCreateScoreData chart
        Screen.Background.load chart.BackgroundPath
        if Audio.changeTrack (chart.AudioPath, chartSaveData.Value.Offset - chart.FirstNote, rate) then
            Audio.playFrom chart.Header.PreviewTime
        options.CurrentChart.Value <- cachedChart.FilePath
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
        layout = options.Playstyles.[keys - 3]
        keycount = keys
    }

    let setScore (data: ScoreInfoProvider) : BestFlags =
        if
            data.ModStatus < ModStatus.Unstored &&
            match options.ScoreSaveCondition.Value with
            | ScoreSaving.Pacemaker ->
                match options.Pacemaker.Value with
                | Accuracy acc -> data.Scoring.Value >= acc
                | Lamp l -> data.Lamp >= l
            | ScoreSaving.PersonalBest -> true // todo: nyi
            | ScoreSaving.Always
            | _ -> true
        then
            // todo: score uploading goes here when implemented
            Scores.saveScore chartSaveData.Value data
        else BestFlags.Default

    let save() =
        Scores.save()
        Library.save()

    let init() =
        try
            let c, ch =
                match Library.lookup options.CurrentChart.Value with
                | Some cc ->
                    match Library.load cc with
                    | Some c -> cc, c
                    | None ->
                        Logging.Error("Could not load chart file: " + cc.FilePath)
                        Library.getGroups (K "All") (Comparison(fun _ _ -> 0)) []
                        |> fun d -> fst d.["All"].[0]
                        |> fun c -> c, Library.load(c).Value
                | None ->
                    Logging.Info("Could not find cached chart: " + options.CurrentChart.Value)
                    Library.getGroups(K "All") (Comparison(fun _ _ -> 0)) []
                    |> fun d -> fst d.["All"].[0]
                    |> fun c -> c, Library.load(c).Value
            changeChart(c, ch)
        with err ->
            Logging.Debug("Tried to auto select a chart but none exist", err)
            Screen.Background.load ""