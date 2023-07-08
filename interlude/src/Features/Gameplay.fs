namespace Interlude.Features

open System
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Prelude
open Prelude.Charts.Formats.Interlude
open Prelude.Charts.Tools
open Prelude.Charts.Tools.NoteColors
open Prelude.Gameplay.Mods
open Prelude.Gameplay
open Prelude.Gameplay.Difficulty
open Prelude.Data.Charts
open Prelude.Data.Charts.Tables
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Collections
open Prelude.Data.Scores
open Interlude
open Interlude.Options
open Interlude.UI
open Interlude.Utils
open Interlude.Features.Online

module Gameplay =

    let mutable autoplay = false
    let mutable enablePacemaker = false
    
    module Chart =
    
        let mutable cacheInfo : CachedChart option = None
        let mutable current : Chart option = None
        let mutable context : LibraryContext = LibraryContext.None
        let mutable withMods : ModChart option = None
        let mutable withColors : ColorizedChart option = None
    
        let mutable rating : RatingReport option = None
        let mutable saveData : ChartSaveData option = None

        let _rate = Setting.rate 1.0f
        let _selectedMods = Setting.simple Map.empty

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
        
        let chartChangeEvent = Event<unit>()
        let onChange = chartChangeEvent.Publish

        let change(cache, ctx, c) =
            cacheInfo <- Some cache
            current <- Some c
            context <- ctx
            saveData <- Some (Scores.getOrCreateData c)
            Background.load c.BackgroundPath
            if Song.change (c.AudioPath, saveData.Value.Offset - c.FirstNote, _rate.Value) then
                Song.playFrom c.Header.PreviewTime
            options.CurrentChart.Value <- cache.FilePath
            update()
            chartChangeEvent.Trigger()

        let colored() =
            match withMods with
            | None -> failwith "Tried to get coloredChart when no modifiedChart exists"
            | Some mc ->
                withColors <- Option.defaultWith (fun () -> getColoredChart (Content.noteskinConfig().NoteColors) mc) withColors |> Some
                withColors.Value

        let recolor() = withColors <- None

        let format_duration() =
            match cacheInfo with
            | Some cc -> cc.Length
            | None -> 0.0f<ms>
            |> fun x -> x / _rate.Value
            |> fun x -> (x / 1000.0f / 60.0f |> int, (x / 1000f |> int) % 60)
            |> fun (x, y) -> sprintf "%s %i:%02i" Icons.time x y

        let format_bpm() =
            match cacheInfo with
            | Some cc -> cc.BPM
            | None -> (500.0f<ms/beat>, 500.0f<ms/beat>)
            |> fun (b, a) -> (60000.0f<ms> / a * _rate.Value |> int, 60000.0f<ms> / b * _rate.Value |> int)
            |> fun (a, b) ->
                if a > 9000 || b < 0 then sprintf "%s ∞" Icons.bpm
                elif Math.Abs(a - b) < 5 || b > 9000 then sprintf "%s %i" Icons.bpm a
                else sprintf "%s %i-%i" Icons.bpm a b

        let format_notecounts() =
            match current with
            | Some c ->
                let mutable notes = 0
                let mutable lnotes = 0
                for { Data = nr } in c.Notes do
                    for n in nr do
                        if n = NoteType.NORMAL then notes <- notes + 1
                        elif n = NoteType.HOLDHEAD then notes <- notes + 1; lnotes <- lnotes + 1
                sprintf "%iK | %i Notes | %.0f%% Holds" c.Keys notes (100.0f * float32 lnotes / float32 notes)
            | None -> ""
    
    module Collections =
        
        open Prelude.Data.Charts.Library
    
        let mutable current : Collection option = None
    
        let notifyChangeRate v =
            match Chart.context with
            | LibraryContext.Playlist (_, _, d) -> d.Rate.Value <- v
            | _ -> ()
    
        let notifyChangeMods mods =
            match Chart.context with
            | LibraryContext.Playlist (_, _, d) -> d.Mods.Value <- mods
            | _ -> ()
    
        let notifyChangeChart (rate: Setting.Bounded<float32>) (mods: Setting<ModState>) =
            match Chart.context with
            | LibraryContext.Playlist (_, _, d) -> 
                rate.Value <- d.Rate.Value
                mods.Value <- d.Mods.Value
            | _ -> ()

        let unselect() =
            options.SelectedCollection.Set None
            current <- None
    
        let select(name: string) =
            match collections.Get name with
            | Some c -> 
                options.SelectedCollection.Set (Some name)
                current <- Some c
            | None -> 
                Logging.Error (sprintf "No such collection with name '%s'" name)
                options.SelectedCollection.Set None

    let rate =
        Chart._rate
        |> Setting.trigger ( fun v ->
                Collections.notifyChangeRate v
                Song.changeRate v
                Chart.update() )
    let selectedMods =
        Chart._selectedMods
        |> Setting.trigger ( fun mods ->
                Collections.notifyChangeMods mods
                Chart.update() )

    do Chart.onChange.Add ( fun () -> Collections.notifyChangeChart rate selectedMods)

    let makeScore (replayData, keys) : Score =
        {
            time = DateTime.Now
            replay = Replay.compress replayData
            rate = rate.Value
            selectedMods = selectedMods.Value |> ModState.filter Chart.withMods.Value
            layout = options.Playstyles.[keys - 3]
            keycount = keys
        }

    let setScore (pacemakerMet: bool) (data: ScoreInfoProvider) : ImprovementFlags =
        if data.ModStatus < ModStatus.Unstored &&
           (options.SaveScoreIfUnderPace.Value || pacemakerMet)
        then
            if data.ModStatus = ModStatus.Ranked then
                // todo: score uploading goes here when online added
                Scores.saveScoreWithPbs Chart.saveData.Value Content.Rulesets.current_hash data
            else
                Scores.saveScore Chart.saveData.Value data
                ImprovementFlags.Default
        else ImprovementFlags.Default

    module Online =
            
        module Multiplayer =
                
            let replays = new Dictionary<string, IScoreMetric * (unit -> ScoreInfoProvider)>()
    
            let private on_leave_lobby() =
                replays.Clear()
    
            let private on_game_start() =
                replays.Clear()
    
            let private player_status(username, status) =
                if status = Web.Shared.Packets.LobbyPlayerStatus.Playing then
                    let chart = Chart.withMods.Value
                    replays.Add(username, 
                        let metric =
                            Metrics.createScoreMetric
                                Content.Rulesets.current
                                chart.Keys
                                Network.lobby.Value.Players.[username].Replay
                                chart.Notes
                                Chart._rate.Value
                        metric,
                        fun () -> 
                            let replay = Network.lobby.Value.Players.[username].Replay
                            if not (replay :> IReplayProvider).Finished then replay.Finish()
                            ScoreInfoProvider(
                                makeScore((replay :> IReplayProvider).GetFullReplay(), chart.Keys),
                                Chart.current.Value,
                                Content.Rulesets.current,
                                Player = Some username
                            )
                    )
    
            let add_own_replay(s: IScoreMetric, replay: LiveReplayProvider) =
                replays.Add(Network.username, 
                    (s, 
                        fun () -> 
                            if not (replay :> IReplayProvider).Finished then replay.Finish()
                            ScoreInfoProvider(
                                makeScore((replay :> IReplayProvider).GetFullReplay(), Chart.withMods.Value.Keys),
                                Chart.current.Value,
                                Content.Rulesets.current
                            )
                    ))
    
            let init () =
                Network.Events.game_start.Add on_game_start
                Network.Events.leave_lobby.Add on_leave_lobby
                Network.Events.player_status.Add player_status

    let save() =
        Scores.save()
        Library.save()
        Stats.save()

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
                        |> fun d -> fst d.[(0, "All")].Charts.[0]
                        |> fun c -> c, Library.load(c).Value
                | None ->
                    Logging.Info("Could not find cached chart: " + options.CurrentChart.Value)
                    Library.getGroups Unchecked.defaultof<_> (K (0, "All")) (Comparison(fun _ _ -> 0)) []
                    |> fun d -> fst d.[(0, "All")].Charts.[0]
                    |> fun c -> c, Library.load(c).Value
            Chart.change(c, LibraryContext.None, ch)
        with err ->
            Logging.Debug("No charts installed")
            Background.load ""
        Table.init(options.Table.Value)
        match options.SelectedCollection.Value with
        | Some c -> Collections.select c
        | None -> ()
        Online.Multiplayer.init()
        Stats.init()