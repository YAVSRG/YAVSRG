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
open Prelude.Data.Charts.Collections
open Prelude.Data.Scores
open Interlude
open Interlude.Options
open Interlude.UI
open Interlude.Utils

module Gameplay =

    module Collections =
        let mutable internal currentChartContext : LevelSelectContext = LevelSelectContext.None
        let mutable contextIndex = -1, ""

        let notifyChangeRate v =
            match currentChartContext with
            | LevelSelectContext.None -> ()
            | LevelSelectContext.Playlist (_, _, d) -> d.Rate.Value <- v
            | LevelSelectContext.Goal (_, _, d) -> d.Rate.Value <- v

        let notifyChangeMods mods =
            match currentChartContext with
            | LevelSelectContext.None -> ()
            | LevelSelectContext.Playlist (_, _, d) -> d.Mods.Value <- mods
            | LevelSelectContext.Goal (_, _, d) -> d.Mods.Value <- mods

        let notifyChangeChart context (rate: Setting.Bounded<float32>) (mods: Setting<ModState>) =
            currentChartContext <- context
            match currentChartContext with
            | LevelSelectContext.None -> ()
            | LevelSelectContext.Playlist (_, _, d) -> 
                rate.Value <- d.Rate.Value
                mods.Value <- d.Mods.Value
            | LevelSelectContext.Goal (_, _, d) ->
                rate.Value <- d.Rate.Value
                mods.Value <- d.Mods.Value
            contextIndex <- currentChartContext.Id

        let reorder (up: bool) : bool =
            match currentChartContext with
            | LevelSelectContext.None
            | LevelSelectContext.Goal _ -> false
            | LevelSelectContext.Playlist (index, id, d) ->
                match Library.Collections.reorderPlaylist id index up with
                | Some newIndex when newIndex <> index ->
                    currentChartContext <- LevelSelectContext.Playlist (newIndex, id, d)
                    contextIndex <- currentChartContext.Id
                    true
                | _ -> false

    let mutable autoplay = false

    let mutable ruleset : Ruleset = getCurrentRuleset()
    let mutable rulesetId = Ruleset.hash ruleset

    
    module Chart =
    
        let mutable cacheInfo : CachedChart option = None
        let mutable current : Chart option = None
        let mutable withMods : ModChart option = None
        let mutable withColors : ColorizedChart option = None
    
        let mutable rating : RatingReport option = None
    
        let mutable saveData : ChartSaveData option = None
        let mutable bests : Bests option = None

        let private _rate = Setting.rate 1.0f
        let private _selectedMods = Setting.simple Map.empty

        let update() =
            match current with
            | None -> ()
            | Some c ->
                let modChart = getModChart _selectedMods.Value c
                withMods <- Some modChart
                rating <- 
                    RatingReport(modChart.Notes, _rate.Value, options.Playstyles.[modChart.Keys - 3], modChart.Keys)
                    |> Some
                withColors <- None

        let rate : Setting.Bounded<float32> = 
            _rate
            |> Setting.trigger ( fun v ->
                    Collections.notifyChangeRate v
                    Audio.changeRate v
                    update() )
        let selectedMods : Setting<ModState> = 
            _selectedMods
            |> Setting.trigger ( fun mods ->
                    Collections.notifyChangeMods mods
                    update() )
        
        let mutable onChange = ignore
        let change(cache, context, c) =
            cacheInfo <- Some cache
            current <- Some c
            saveData <- Some (Scores.getOrCreateScoreData c)
            Collections.notifyChangeChart context rate selectedMods
            Screen.Background.load c.BackgroundPath
            if Audio.changeTrack (c.AudioPath, saveData.Value.Offset - c.FirstNote, rate.Value) then
                Audio.playFrom c.Header.PreviewTime
            options.CurrentChart.Value <- cache.FilePath
            update()
            onChange()

        let colored() =
            match withMods with
            | None -> failwith "Tried to get coloredChart when no modifiedChart exists"
            | Some mc ->
                withColors <- Option.defaultWith (fun () -> getColoredChart (Content.noteskinConfig().NoteColors) mc) withColors |> Some
                withColors.Value

        let recolor() = withColors <- None

    let rate = Chart.rate
    let selectedMods = Chart.selectedMods

    let makeScore (replayData, keys) : Score =
        {
            time = DateTime.Now
            replay = Replay.compress replayData
            rate = rate.Value
            selectedMods = selectedMods.Value |> ModChart.filter Chart.withMods.Value
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
            Scores.saveScore Chart.saveData.Value rulesetId data
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
                        Library.getGroups Unchecked.defaultof<_> (K (0, "All")) (Comparison(fun _ _ -> 0)) []
                        |> fun d -> fst d.[(0, "All")].[0]
                        |> fun c -> c, Library.load(c).Value
                | None ->
                    Logging.Info("Could not find cached chart: " + options.CurrentChart.Value)
                    Library.getGroups Unchecked.defaultof<_> (K (0, "All")) (Comparison(fun _ _ -> 0)) []
                    |> fun d -> fst d.[(0, "All")].[0]
                    |> fun c -> c, Library.load(c).Value
            Chart.change(c, LevelSelectContext.None, ch)
        with err ->
            Logging.Debug("Tried to auto select a chart but none exist", err)
            Screen.Background.load ""