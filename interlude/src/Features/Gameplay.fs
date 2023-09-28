namespace Interlude.Features

open System
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts.Formats.Interlude
open Prelude.Charts.Tools
open Prelude.Charts.Tools.NoteColors
open Prelude.Charts.Tools.Patterns
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
open Interlude.Utils
open Interlude.UI
open Interlude.Features.Stats
open Interlude.Features.Online
open Interlude.Web.Shared

module Gameplay =

    let mutable autoplay = false
    let mutable enablePacemaker = false
    
    module Chart =
    
        let _rate = Setting.rate 1.0f
        let _selectedMods = Setting.simple Map.empty

        let private format_duration(cc: CachedChart option) =
            match cc with
            | Some cc -> cc.Length
            | None -> 0.0f<ms>
            |> fun x -> x / _rate.Value
            |> fun x -> (x / 1000.0f / 60.0f |> int, (x / 1000f |> int) % 60)
            |> fun (x, y) -> sprintf "%s %i:%02i" Icons.time x y
        
        let private format_bpm(cc: CachedChart option) =
            match cc with
            | Some cc -> cc.BPM
            | None -> (500.0f<ms/beat>, 500.0f<ms/beat>)
            |> fun (b, a) -> (60000.0f<ms> / a * _rate.Value |> int, 60000.0f<ms> / b * _rate.Value |> int)
            |> fun (a, b) ->
                if a > 9000 || b < 0 then sprintf "%s ∞" Icons.bpm
                elif Math.Abs(a - b) < 5 || b > 9000 then sprintf "%s %i" Icons.bpm a
                else sprintf "%s %i-%i" Icons.bpm a b
        
        let private format_notecounts(chart: Chart) =
            let mutable notes = 0
            let mutable lnotes = 0
            for { Data = nr } in chart.Notes do
                for n in nr do
                    if n = NoteType.NORMAL then notes <- notes + 1
                    elif n = NoteType.HOLDHEAD then notes <- notes + 1; lnotes <- lnotes + 1
            sprintf "%iK | %i Notes | %.0f%% Holds" chart.Keys notes (100.0f * float32 lnotes / float32 notes)

        let mutable CACHE_DATA : CachedChart option = None
        let mutable FMT_DURATION : string = format_duration None
        let mutable FMT_BPM : string = format_bpm None
        let mutable LIBRARY_CTX = LibraryContext.None

        let mutable CHART : Chart option = None
        let mutable SAVE_DATA : ChartSaveData option = None

        let mutable WITH_MODS : ModChart option = None
        let mutable FMT_NOTECOUNTS : string option = None
        let mutable RATING : RatingReport option = None
        let mutable PATTERNS : Patterns.PatternReportEntry list option = None

        let mutable WITH_COLORS : ColorizedChart option = None

        let not_selected() = CACHE_DATA.IsNone
        let is_loading() = CACHE_DATA.IsSome && CHART.IsNone
        let is_loaded() = CACHE_DATA.IsSome && CHART.IsSome
        
        let private chart_change_ev = Event<unit>()
        let on_chart_change = chart_change_ev.Publish

        let mutable on_load_finished = []

        type private LoadRequest = Load of CachedChart | Update of bool | Recolor
        let private chart_loader =
            { new Async.SwitchServiceSeq<LoadRequest, unit -> unit>() with
                override this.Process(req) =
                    match req with
                    | Load cc ->
                        seq {
                            match Cache.load cc Library.cache with
                            | None ->
                                // set error state
                                Notifications.error(L"notification.chart_load_failed.title", L"notification.chart_load_failed.body")
                                Background.load None
                            | Some chart ->

                            Background.load (Cache.background_path chart Library.cache)

                            let save_data = Scores.getOrCreateData chart

                            yield fun () -> 
                                CHART <- Some chart
                                Song.change(
                                    Cache.audio_path chart Library.cache, 
                                    save_data.Offset - chart.FirstNote,
                                    _rate.Value,
                                    (chart.Header.PreviewTime, chart.LastNote)
                                )
                                SAVE_DATA <- Some save_data
                            // if chart is loaded we can safely restart from this point for different rates and mods

                            let with_mods = getModChart _selectedMods.Value chart
                            let with_colors = getColoredChart (Content.noteskinConfig().NoteColors) with_mods
                            let rating = RatingReport(with_mods.Notes, _rate.Value, options.Playstyles.[with_mods.Keys - 3], with_mods.Keys)
                            let patterns = Patterns.generate_pattern_report (_rate.Value, chart)
                            let note_counts = format_notecounts chart

                            yield fun () ->
                                WITH_MODS <- Some with_mods
                                WITH_COLORS <- Some with_colors
                                RATING <- Some rating
                                PATTERNS <- Some patterns
                                FMT_NOTECOUNTS <- Some note_counts
                                chart_change_ev.Trigger()

                            yield fun () ->
                                for action in on_load_finished do 
                                    action()
                                on_load_finished <- []
                        }
                    | Update is_interrupted_load ->
                        seq {
                            match CHART with
                            | None -> failwith "impossible"
                            | Some chart ->

                            let with_mods = getModChart _selectedMods.Value chart
                            let with_colors = getColoredChart (Content.noteskinConfig().NoteColors) with_mods
                            let rating = RatingReport(with_mods.Notes, _rate.Value, options.Playstyles.[with_mods.Keys - 3], with_mods.Keys)
                            let patterns = Patterns.generate_pattern_report (_rate.Value, chart)
                            let note_counts = format_notecounts chart

                            yield fun () ->
                                WITH_MODS <- Some with_mods
                                WITH_COLORS <- Some with_colors
                                RATING <- Some rating
                                PATTERNS <- Some patterns
                                FMT_NOTECOUNTS <- Some note_counts
                                if is_interrupted_load then chart_change_ev.Trigger()

                            yield fun () ->
                                for action in on_load_finished do 
                                    action()
                                on_load_finished <- []
                        }
                    | Recolor ->
                        seq {
                            match WITH_MODS with
                            | None -> failwith "impossible"
                            | Some with_mods ->
                            
                            let with_colors = getColoredChart (Content.noteskinConfig().NoteColors) with_mods
                            yield fun () -> WITH_COLORS <- Some with_colors
                            
                            yield fun () ->
                                for action in on_load_finished do 
                                    action()
                                on_load_finished <- []
                        }
                override this.Handle(action) = action()
            }

        let change(cc: CachedChart, ctx: LibraryContext) =
            CACHE_DATA <- Some cc
            FMT_DURATION <- format_duration CACHE_DATA
            FMT_BPM <- format_bpm CACHE_DATA
            LIBRARY_CTX <- ctx
            options.CurrentChart.Value <- cc.Key

            CHART <- None
            SAVE_DATA <- None

            WITH_MODS <- None
            FMT_NOTECOUNTS <- None
            RATING <- None
            PATTERNS <- None

            WITH_COLORS <- None

            chart_loader.Request(Load cc)

        let update() =
            if CHART.IsSome then
                
                let is_interrupted = WITH_MODS.IsNone

                WITH_MODS <- None
                FMT_NOTECOUNTS <- None
                RATING <- None
                PATTERNS <- None

                WITH_COLORS <- None

                chart_loader.Request (Update is_interrupted)
                
        let recolor() =
            if WITH_MODS.IsSome then
                
                WITH_COLORS <- None

                chart_loader.Request Recolor

        let wait_for_load(action) =
            if CACHE_DATA.IsNone then ()
            elif WITH_COLORS.IsSome then action()
            else on_load_finished <- action :: on_load_finished

        do sync_forever chart_loader.Join
    
    module Collections =
        
        open Prelude.Data.Charts.Library
    
        let mutable current : Collection option = None
    
        let notifyChangeRate v =
            match Chart.LIBRARY_CTX with
            | LibraryContext.Playlist (_, _, d) -> d.Rate.Value <- v
            | _ -> ()
    
        let notifyChangeMods mods =
            match Chart.LIBRARY_CTX with
            | LibraryContext.Playlist (_, _, d) -> d.Mods.Value <- mods
            | _ -> ()
    
        let notifyChangeChart (rate: Setting.Bounded<float32>) (mods: Setting<ModState>) =
            match Chart.LIBRARY_CTX with
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

    do Chart.on_chart_change.Add ( fun () -> Collections.notifyChangeChart rate selectedMods)

    let makeScore (replayData, keys) : Score =
        {
            time = DateTime.UtcNow
            replay = Replay.compress replayData
            rate = rate.Value
            selectedMods = selectedMods.Value |> ModState.filter Chart.WITH_MODS.Value
            layout = options.Playstyles.[keys - 3]
            keycount = keys
        }

    let setScore (pacemakerMet: bool) (data: ScoreInfoProvider) : ImprovementFlags =
        if data.ModStatus < ModStatus.Unstored &&
           (options.SaveScoreIfUnderPace.Value || pacemakerMet)
        then
            if data.ModStatus = ModStatus.Ranked then
                if Network.status = Network.Status.LoggedIn then
                    API.Client.post("charts/scores", 
                        ({ 
                            ChartId = Chart.CACHE_DATA.Value.Hash
                            Replay = data.ScoreInfo.replay
                            Rate = data.ScoreInfo.rate
                            Mods = data.ScoreInfo.selectedMods
                            Timestamp = data.ScoreInfo.time
                        }: Requests.Charts.Scores.Save.Request), ignore)
                Scores.saveScoreWithPbs Chart.SAVE_DATA.Value Content.Rulesets.current_hash data
            else
                Scores.saveScore Chart.SAVE_DATA.Value data.ScoreInfo
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
                if status = LobbyPlayerStatus.Playing then

                    Chart.wait_for_load <| fun () ->

                    let chart = Chart.CHART.Value
                    let with_mods = Chart.WITH_MODS.Value
                    let replay = Network.lobby.Value.Players.[username].Replay
                    replays.Add(username, 
                        let metric =
                            Metrics.createScoreMetric
                                Content.Rulesets.current
                                with_mods.Keys
                                replay
                                with_mods.Notes
                                Chart._rate.Value
                        metric,
                        fun () ->
                            if not (replay :> IReplayProvider).Finished then replay.Finish()
                            ScoreInfoProvider(
                                makeScore((replay :> IReplayProvider).GetFullReplay(), with_mods.Keys),
                                chart,
                                Content.Rulesets.current,
                                Player = Some username
                            )
                    )
    
            let add_own_replay(s: IScoreMetric, replay: LiveReplayProvider) =
                let chart = Chart.CHART.Value
                let with_mods = Chart.WITH_MODS.Value
                replays.Add(Network.credentials.Username, 
                    (s, 
                        fun () -> 
                            if not (replay :> IReplayProvider).Finished then replay.Finish()
                            ScoreInfoProvider(
                                makeScore((replay :> IReplayProvider).GetFullReplay(), with_mods.Keys),
                                chart,
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
            let cc =
                match Cache.by_key options.CurrentChart.Value Library.cache with
                | Some cc -> cc
                | None ->
                    Logging.Info("Could not find cached chart: " + options.CurrentChart.Value)
                    Suggestions.Suggestion.get_random []
            Chart.change(cc, LibraryContext.None)
        with err ->
            Logging.Debug "No charts installed"
            Background.load None
        Table.init options.Table.Value
        match options.SelectedCollection.Value with
        | Some c -> Collections.select c
        | None -> ()
        Online.Multiplayer.init()
        Stats.init()