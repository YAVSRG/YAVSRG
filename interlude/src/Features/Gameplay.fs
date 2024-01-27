namespace Interlude.Features

open System
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts
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
open Interlude.Features.Online
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests

module Gameplay =

    let mutable autoplay = false
    let endless_mode = Setting.simple false

    module Chart =

        let _rate = Setting.rate 1.0f
        let _selected_mods = options.SelectedMods

        let private format_duration (cc: CachedChart option) =
            match cc with
            | Some cc -> cc.Length
            | None -> 0.0f<ms>
            |> fun x -> x / _rate.Value
            |> fun x -> (x / 1000.0f / 60.0f |> int, (x / 1000f |> int) % 60)
            |> fun (x, y) -> sprintf "%s %i:%02i" Icons.CLOCK x y

        let private format_bpm (cc: CachedChart option) =
            match cc with
            | Some cc -> cc.BPM
            | None -> (500.0f<ms / beat>, 500.0f<ms / beat>)
            |> fun (b, a) -> (60000.0f<ms> / a * _rate.Value |> int, 60000.0f<ms> / b * _rate.Value |> int)
            |> fun (a, b) ->
                if a > 9000 || b < 0 then
                    sprintf "%s ∞" Icons.MUSIC
                elif Math.Abs(a - b) < 5 || b > 9000 then
                    sprintf "%s %i" Icons.MUSIC a
                else
                    sprintf "%s %i-%i" Icons.MUSIC a b

        let private format_notecounts (chart: ModdedChart) =
            let mutable notes = 0
            let mutable lnotes = 0

            for { Data = nr } in chart.Notes do
                for n in nr do
                    if n = NoteType.NORMAL then
                        notes <- notes + 1
                    elif n = NoteType.HOLDHEAD then
                        notes <- notes + 1
                        lnotes <- lnotes + 1

            let hold_count =
                let pc = (100.0f * float32 lnotes / float32 notes)

                if pc < 0.5f then
                    sprintf "%i Holds" lnotes
                else
                    sprintf "%.0f%% Holds" pc

            sprintf "%iK | %i Notes | %s" chart.Keys notes hold_count

        let mutable CACHE_DATA: CachedChart option = None
        let mutable FMT_DURATION: string = format_duration None
        let mutable FMT_BPM: string = format_bpm None
        let mutable LIBRARY_CTX = LibraryContext.None

        let mutable CHART: Chart option = None
        let mutable SAVE_DATA: ChartSaveData option = None

        let mutable WITH_MODS: ModdedChart option = None
        let mutable FMT_NOTECOUNTS: string option = None
        let mutable RATING: RatingReport option = None
        let mutable PATTERNS: Patterns.PatternReport option = None

        let mutable WITH_COLORS: ColoredChart option = None

        type LoadingChartInfo =
            {
                CacheInfo: CachedChart
                LibraryContext: LibraryContext
                DurationString: string
                BpmString: string
            }

        type LoadedChartInfo =
            {
                CacheInfo: CachedChart
                LibraryContext: LibraryContext
                DurationString: string
                BpmString: string

                Chart: Chart
                SaveData: ChartSaveData

                WithMods: ModdedChart
                NotecountsString: string
                Rating: RatingReport
                Patterns: Patterns.PatternReport

                WithColors: ColoredChart
            }

        let private chart_change_finished = Event<unit>()
        let on_chart_change_finished = chart_change_finished.Publish
        let private chart_change_started = Event<LoadingChartInfo>()
        let on_chart_change_started = chart_change_started.Publish

        let mutable private on_load_succeeded = []

        type private LoadRequest =
            | Load of CachedChart * play_audio: bool
            | Update of bool
            | Recolor

        let private chart_loader =
            { new Async.SwitchServiceSeq<LoadRequest, unit -> unit>() with
                override this.Process(req) =
                    match req with
                    | Load(cc, play_audio) ->
                        seq {
                            match Cache.load cc Library.cache with
                            | None ->
                                // todo: set a proper error state to indicate this failed
                                Background.load None

                                Notifications.error (
                                    %"notification.chart_load_failed.title",
                                    %"notification.chart_load_failed.body"
                                )

                                yield
                                    fun () ->
                                        chart_change_finished.Trigger()
                                        on_load_succeeded <- []
                            | Some chart ->

                            Background.load (Cache.background_path chart Library.cache)

                            let save_data = Scores.get_or_create chart

                            yield
                                fun () ->
                                    CHART <- Some chart

                                    Song.change (
                                        Cache.audio_path chart Library.cache,
                                        save_data.Offset - chart.FirstNote,
                                        _rate.Value,
                                        (chart.Header.PreviewTime, chart.LastNote),
                                        play_audio
                                    )

                                    SAVE_DATA <- Some save_data
                            // if chart is loaded we can safely restart from this point for different rates and mods

                            let with_mods = apply_mods _selected_mods.Value chart
                            let with_colors = apply_coloring (Content.noteskin_config().NoteColors) with_mods

                            let rating =
                                RatingReport(
                                    with_mods.Notes,
                                    _rate.Value,
                                    options.Playstyles.[with_mods.Keys - 3],
                                    with_mods.Keys
                                )

                            let patterns = Patterns.generate_pattern_report (_rate.Value, chart)
                            let note_counts = format_notecounts with_mods

                            yield
                                fun () ->
                                    WITH_MODS <- Some with_mods
                                    WITH_COLORS <- Some with_colors
                                    RATING <- Some rating
                                    PATTERNS <- Some patterns
                                    FMT_NOTECOUNTS <- Some note_counts
                                    chart_change_finished.Trigger()

                            yield
                                fun () ->
                                    for action in on_load_succeeded do
                                        action ()

                                    on_load_succeeded <- []
                        }
                    | Update is_interrupted_load ->
                        seq {
                            match CHART with
                            | None -> ()
                            | Some chart ->

                            let with_mods = apply_mods _selected_mods.Value chart
                            let with_colors = apply_coloring (Content.noteskin_config().NoteColors) with_mods

                            let rating =
                                RatingReport(
                                    with_mods.Notes,
                                    _rate.Value,
                                    options.Playstyles.[with_mods.Keys - 3],
                                    with_mods.Keys
                                )

                            let patterns = Patterns.generate_pattern_report (_rate.Value, chart)
                            let note_counts = format_notecounts with_mods

                            yield
                                fun () ->
                                    WITH_MODS <- Some with_mods
                                    WITH_COLORS <- Some with_colors
                                    RATING <- Some rating
                                    PATTERNS <- Some patterns
                                    FMT_NOTECOUNTS <- Some note_counts

                                    if is_interrupted_load then
                                        chart_change_finished.Trigger()

                            yield
                                fun () ->
                                    for action in on_load_succeeded do
                                        action ()

                                    on_load_succeeded <- []
                        }
                    | Recolor ->
                        seq {
                            match WITH_MODS with
                            | None -> ()
                            | Some with_mods ->

                            let with_colors = apply_coloring (Content.noteskin_config().NoteColors) with_mods
                            yield fun () -> WITH_COLORS <- Some with_colors

                            yield
                                fun () ->
                                    for action in on_load_succeeded do
                                        action ()

                                    on_load_succeeded <- []
                        }

                override this.Handle(action) = action ()
            }

        let change (cc: CachedChart, ctx: LibraryContext, auto_play_audio: bool) =
            // todo: show a one-time warning if chart loading takes over 1 second
            CACHE_DATA <- Some cc
            LIBRARY_CTX <- ctx
            options.CurrentChart.Value <- cc.Key

            CHART <- None
            SAVE_DATA <- None

            WITH_MODS <- None
            FMT_NOTECOUNTS <- None
            RATING <- None
            PATTERNS <- None

            WITH_COLORS <- None

            FMT_DURATION <- format_duration CACHE_DATA
            FMT_BPM <- format_bpm CACHE_DATA

            chart_change_started.Trigger {
                CacheInfo = cc
                LibraryContext = LIBRARY_CTX
                DurationString = FMT_DURATION
                BpmString = FMT_BPM
            }

            chart_loader.Request(Load(cc, auto_play_audio))

        let update () =
            if CACHE_DATA.IsSome then

                FMT_DURATION <- format_duration CACHE_DATA
                FMT_BPM <- format_bpm CACHE_DATA

            if CHART.IsSome then

                let is_interrupted = WITH_MODS.IsNone

                WITH_MODS <- None
                FMT_NOTECOUNTS <- None
                RATING <- None
                PATTERNS <- None

                WITH_COLORS <- None

                chart_loader.Request(Update is_interrupted)

        let recolor () =
            if WITH_MODS.IsSome then

                WITH_COLORS <- None

                chart_loader.Request Recolor

        let if_loading (action: LoadingChartInfo -> unit) =
            if CACHE_DATA.IsSome then
                action {
                    CacheInfo = CACHE_DATA.Value
                    LibraryContext = LIBRARY_CTX
                    DurationString = FMT_DURATION
                    BpmString = FMT_BPM
                }

        let if_loaded (action: LoadedChartInfo -> unit) =
            if WITH_COLORS.IsSome then 
                action {
                    CacheInfo = CACHE_DATA.Value
                    LibraryContext = LIBRARY_CTX
                    DurationString = FMT_DURATION
                    BpmString = FMT_BPM

                    Chart = CHART.Value
                    SaveData = SAVE_DATA.Value

                    WithMods = WITH_MODS.Value
                    NotecountsString = FMT_NOTECOUNTS.Value
                    Rating = RATING.Value
                    Patterns = PATTERNS.Value

                    WithColors = WITH_COLORS.Value
                }

        let when_loaded (action: LoadedChartInfo -> unit) =
            if WITH_COLORS.IsSome then
                action {
                    CacheInfo = CACHE_DATA.Value
                    LibraryContext = LIBRARY_CTX
                    DurationString = FMT_DURATION
                    BpmString = FMT_BPM

                    Chart = CHART.Value
                    SaveData = SAVE_DATA.Value

                    WithMods = WITH_MODS.Value
                    NotecountsString = FMT_NOTECOUNTS.Value
                    Rating = RATING.Value
                    Patterns = PATTERNS.Value

                    WithColors = WITH_COLORS.Value
                }
            else
                on_load_succeeded <- (fun () -> 
                    action {
                        CacheInfo = CACHE_DATA.Value
                        LibraryContext = LIBRARY_CTX
                        DurationString = FMT_DURATION
                        BpmString = FMT_BPM

                        Chart = CHART.Value
                        SaveData = SAVE_DATA.Value

                        WithMods = WITH_MODS.Value
                        NotecountsString = FMT_NOTECOUNTS.Value
                        Rating = RATING.Value
                        Patterns = PATTERNS.Value

                        WithColors = WITH_COLORS.Value
                    }
                ) :: on_load_succeeded

        let init_window() = sync_forever chart_loader.Join

    let collections_on_rate_changed (library_ctx: LibraryContext) (v: float32) =
        match library_ctx with
        | LibraryContext.Playlist(_, _, d) -> d.Rate.Value <- v
        | _ -> ()

    let collections_on_mods_changed (library_ctx: LibraryContext) (mods: ModState) =
        match library_ctx with
        | LibraryContext.Playlist(_, _, d) -> d.Mods.Value <- mods
        | _ -> ()

    let collections_on_chart_changed (library_ctx: LibraryContext) (rate: Setting.Bounded<float32>) (mods: Setting<ModState>) =
        match library_ctx with
        | LibraryContext.Playlist(_, _, d) ->
            rate.Value <- d.Rate.Value
            mods.Value <- d.Mods.Value
        | _ -> ()

    let presets_on_chart_changed =
        let mutable previous_keymode = None
        fun cc ->
            match previous_keymode with
            | Some k when k <> cc.Keys -> Presets.keymode_changed cc.Keys
            | _ -> ()
            previous_keymode <- Some cc.Keys

    let rate =
        Chart._rate
        |> Setting.trigger (fun v ->
            Chart.if_loading <| fun info ->
            collections_on_rate_changed info.LibraryContext v
            Song.change_rate v
            Chart.update ()
        )

    let selected_mods =
        Chart._selected_mods
        |> Setting.trigger (fun mods ->
            Chart.if_loading <| fun info ->
            collections_on_mods_changed info.LibraryContext mods
            Chart.update ()
        )

    let make_score (replay_data, keys) : Score =
        {
            time = DateTime.UtcNow
            replay = Replay.compress_string replay_data
            rate = rate.Value
            selectedMods = selected_mods.Value |> ModState.filter Chart.WITH_MODS.Value
            layout = options.Playstyles.[keys - 3]
            keycount = keys
        }

    let set_score (met_pacemaker: bool) (data: ScoreInfoProvider) : ImprovementFlags =
        if
            data.ModStatus < ModStatus.Unstored
            && (options.SaveScoreIfUnderPace.Value || met_pacemaker)
        then
            if data.ModStatus = ModStatus.Ranked then
                if Network.status = Network.Status.LoggedIn then
                    Charts.Scores.Save.post (
                        ({
                            ChartId = Chart.hash data.Chart // todo: ScoreInfoProvider should be less poo and provide things like this
                            Replay = data.ScoreInfo.replay
                            Rate = data.ScoreInfo.rate
                            Mods = data.ScoreInfo.selectedMods
                            Timestamp = data.ScoreInfo.time
                        }),
                        ignore
                    )

                Scores.save_score_pbs Chart.SAVE_DATA.Value Content.Rulesets.current_hash data
            else
                Scores.save_score Chart.SAVE_DATA.Value data.ScoreInfo
                ImprovementFlags.Default
        else
            ImprovementFlags.Default

    module Multiplayer =

        let replays = new Dictionary<string, IScoreMetric * (unit -> ScoreInfoProvider)>()

        let private on_leave_lobby () = replays.Clear()

        let private on_game_start () = replays.Clear()

        let private player_status (username, status) =
            if status = LobbyPlayerStatus.Playing then

                Chart.when_loaded
                <| fun info ->

                    let replay = Network.lobby.Value.Players.[username].Replay

                    replays.Add(
                        username,
                        let metric =
                            Metrics.create
                                Content.Rulesets.current
                                info.WithMods.Keys
                                replay
                                info.WithMods.Notes
                                Chart._rate.Value

                        metric,
                        fun () ->
                            if not (replay :> IReplayProvider).Finished then
                                replay.Finish()

                            let score =
                                {
                                    time = DateTime.UtcNow
                                    replay = Replay.compress_string ((replay :> IReplayProvider).GetFullReplay())
                                    rate = rate.Value
                                    selectedMods = selected_mods.Value |> ModState.filter info.WithMods
                                    layout = options.Playstyles.[info.WithMods.Keys - 3]
                                    keycount = info.WithMods.Keys
                                }

                            ScoreInfoProvider(
                                score,
                                info.Chart,
                                Content.Rulesets.current,
                                Player = Some username,
                                ModChart = info.WithMods
                            )
                    )

        let add_own_replay (chart: Chart, with_mods: ModdedChart, s: IScoreMetric, replay: LiveReplayProvider) =

            replays.Add(
                Network.credentials.Username,
                (s, fun () ->
                    if not (replay :> IReplayProvider).Finished then
                        replay.Finish()

                    let score =
                        {
                            time = DateTime.UtcNow
                            replay = Replay.compress_string ((replay :> IReplayProvider).GetFullReplay())
                            rate = rate.Value
                            selectedMods = selected_mods.Value |> ModState.filter with_mods
                            layout = options.Playstyles.[with_mods.Keys - 3]
                            keycount = with_mods.Keys
                        }

                    ScoreInfoProvider(
                        score,
                        chart,
                        Content.Rulesets.current,
                        ModChart = with_mods
                    )
                )
            )

        let init_window () =
            Network.Events.game_start.Add on_game_start
            Network.Events.leave_lobby.Add on_leave_lobby
            Network.Events.player_status.Add player_status

    let init_window () =
        match Cache.by_key options.CurrentChart.Value Library.cache with
        | Some cc -> Chart.change (cc, LibraryContext.None, true)
        | None ->
            Logging.Info("Could not find cached chart: " + options.CurrentChart.Value)

            match Suggestions.Suggestion.get_random [] with
            | Some cc -> Chart.change (cc, LibraryContext.None, true)
            | None ->
                Logging.Debug "No charts installed"
                Background.load None

        Table.init_window options.Table.Value
        Multiplayer.init_window ()
        Chart.init_window()

        Chart.on_chart_change_started.Add(fun info -> collections_on_chart_changed info.LibraryContext Chart._rate Chart._selected_mods)
        Chart.on_chart_change_started.Add(fun info -> presets_on_chart_changed info.CacheInfo)