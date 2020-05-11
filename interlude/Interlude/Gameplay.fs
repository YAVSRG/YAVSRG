namespace Interlude

open System
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Editor
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Score
open Prelude.Data.ChartManager
open Prelude.Data.ScoreManager
open Interlude
open Interlude.Utils

module Gameplay =
    
    let mutable internal currentChart: Chart option = None
    let mutable internal currentCachedChart: CachedChart option = None
    let mutable internal chartSaveData = None
    let mutable modifiedChart: Lazy<ModChart> = lazy ( failwith "tried to access modified chart when none is selected" )
    let mutable replayData: Lazy<ScoreData> = lazy ( null )
    let mutable rate = 1.0

    let selectedMods = ModState()
    let difficultyRating = null
    let scores = ScoresDB()
    let cache = Cache()

    let updateChart() =
        match currentChart with
        | None -> ()
        | Some c ->
            let (m, r) = getModChart selectedMods c
            modifiedChart <- m
            replayData <- r
        //force modchart, make lazy coloring
        //run diff calc on uncolored modchart

    let changeChart(cachedChart, chart) =
        currentCachedChart <- Some cachedChart
        currentChart <- Some chart
        chartSaveData <- Some <| scores.GetScoreData(chart)
        //load bg
        let localOffset = if chart.Notes.IsEmpty then 0.0 else chartSaveData.Value.Offset.Get() - (offsetOf chart.Notes.First)
        Audio.changeTrack(chart.AudioPath, localOffset, rate)

    let save() =
        scores.Save()
        cache.Save()
        
    let init() =
        cache.ConvertSongFolder @"C:\Users\percy\Downloads\Singles\STEPPERZ (Tim)" "Singles" (fun x -> Logging.Debug(x) "") |> ignore
        cache.GetGroups(K "All") (new Comparison<CachedChart>(fun _ _ -> 0))
        |> fun d -> d.["All"].[0]
        |> fun c ->
            let ch = cache.LoadChart(c).Value
            changeChart(c, ch)
            Audio.playFrom(ch.Header.PreviewTime)